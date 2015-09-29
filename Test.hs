module Test where

import qualified Crypto.Enigma as CE

import Enigma.Base
import Enigma.Device
import Enigma.Parse

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

