module Test where

import qualified Crypto.Enigma as CE

import Enigma.Base
import Enigma.Device
import Enigma.Parse
import Data.Char
import Data.List

lowerToUpper ch = chr( ord('A') + ord(ch) - ord('a') )
upperToLower ch = chr( ord('a') + ord(ch) - ord('A') )

ce_encode :: CE.EnigmaConfig -> String -> (CE.EnigmaConfig, String)
ce_encode = mapAccumL singleEncode

singleEncode :: CE.EnigmaConfig -> Char -> (CE.EnigmaConfig, Char)
singleEncode cfg ch | 'A' <= ch && ch <= 'Z' = let [b] = CE.enigmaEncoding cfg [ch]
                                               in (CE.step cfg, b)
                    | 'a' <= ch && ch <= 'z' = let [b] = CE.enigmaEncoding cfg [lowerToUpper ch]
                                               in (CE.step cfg, upperToLower b)
                    | otherwise              = (cfg, ch)

toState :: CE.EnigmaConfig -> EnigmaState
toState cfg = EnigmaState r m l
  where (_ : r : m : l : _ ) = CE.positions cfg

testStepping :: CE.EnigmaConfig -> Stepper -> Int -> Bool
testStepping cfg stepper n = all check $ take n (zip csteps dsteps)
  where state = toState cfg
        csteps = iterate CE.step cfg
        dsteps = iterate stepper state
        check (config,state) = toState config == state

cfg = CE.configEnigma "b-β-V-VIII-III" "XQVI" "UX.MO.KZ.AY.EF.PL" "03.17.24.11"

pkg = let Right p = createEnigma    "b-β-V-VIII-III" "XQVI" "UX.MO.KZ.AY.EF.PL" "03.17.24.11"
      in p

-- test that the initial states are the same
test1 = toState cfg == (_state pkg)

-- test the stepping
test2 = testStepping cfg (_stepper pkg) 1000

-- compare encoding a message
test3 msg =
  let enc1 = CE.enigmaEncoding cfg msg
      state = toState cfg
      (_, enc2) = (_encoder pkg) state msg
  in (enc1 == enc2, enc1, enc2)

test4 = 
  let cfg1 = CE.configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11"
      pkg1 = createEnigma    "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11"
      Right (EnigmaPackage device state stepper encoder) = pkg1

      enc1 = CE.enigmaEncoding  cfg1 "KRIEG"
      (_, enc2) = encoder state "KRIEG"
  in (enc1 == enc2, enc1, enc2)

test5 plaintext =
  let Right pkg = createEnigma "B--I-II-III" "AAAA" "" "1 1 1 1"
      (_,ciphertext) = (_encoder pkg) (_state pkg) plaintext
  in ciphertext

test6 plaintext =
  let Right pkg = createEnigma "C-id-I-II-III" "AAAA" "" "1 1 1 1"
      (_,ciphertext) = (_encoder pkg) (_state pkg) plaintext
  in ciphertext

test7 plaintext =
  let Right pkg = createEnigma "A-id-I-II-III" "AAAA" "" "1 1 1 1"
      (_,ciphertext) = (_encoder pkg) (_state pkg) plaintext
  in ciphertext

compareEnigmas rotors window stecker rings plaintext =
  let cfg       = CE.configEnigma rotors window stecker rings
      Right pkg = createEnigma rotors window stecker rings
      (_, enc1) = ce_encode cfg plaintext
      (_, enc2) = (_encoder pkg) (_state pkg) plaintext
  in (enc1 == enc2, enc1, enc2)

test8 = compareEnigmas "B--I-III-II" "ABCD" "" "5.4.1.3"

