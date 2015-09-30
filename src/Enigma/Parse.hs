module Enigma.Parse where

import Enigma.Base
import Enigma.Device

import Data.List
import Data.List.Split
import Text.Show.Pretty
import qualified Data.Char as Char

data EnigmaPackage = EnigmaPackage { _device  :: EnigmaDevice
                                   , _state   :: EnigmaState
                                   , _stepper :: Stepper
                                   , _encoder :: MessageEncoder
                                   }

instance Show EnigmaPackage where
  show package = "EnigmaPackage { _device = " ++ show (_device package)
                       ++ ", _state = " ++ show (_state package)
                       ++ ", _stepper = \"...\", _encoder = \"...\" }"

assert :: Bool -> String -> Either String ()
assert b msg = if not b then Left msg else return ()

isLetter ch = 'A' <= ch && ch <= 'Z'
isPosition p = 1 <= p && p <= 26

createEnigma :: String     -- rotor components
             -> String     -- window letters
             -> String     -- plug board
             -> String     -- ring settings
             -> Either String EnigmaPackage
createEnigma comps window plugstr ringsstr = do
  assert (length window == 4)   "window must be exactly 4 characters"
  assert (all isLetter window)  "window contains non-letter characters"

  let rings = map read (wordsBy (not . Char.isDigit) ringsstr) :: [Int]
  assert (length rings == 4)    "rings must contain exactly 4 positions"
  assert (all isPosition rings) "rings contains non-position [1..26]"

  pairs <- parsePlugboard plugstr

  let rotorNames = sepBy "-"  comps
  rotors <- mapM findRotor rotorNames
  assert (length rotorNames == 5) ("must specify exactly 5 rotar names: " ++ show rotorNames)

  let [reflector, fourth, left, middle, right] = rotors
      [r4, rleft, rmid, rright] = rings
      [w4, wleft, wmid, wright] = window

  let windowPosition' :: Char -> Position -> Position
      windowPosition' ch ring = 1 + mod (letterOrd ch - ring + 1) 26

  let device = EnigmaDevice
               { _stecker = makeCipherFromPairs pairs
               , _etw     = identityCipher
               , _right   = makeWheel right  rright
               , _middle  = makeWheel middle rmid
               , _left    = makeWheel left   rleft
               , _fourth  = makeWheel fourth r4
               , _pos4    = windowPosition'  w4 r4
               , _ukw     = makePerm (fst reflector)
               }

      state = EnigmaState r m l
        where r = windowPosition' wright rright
              m = windowPosition' wmid   rmid
              l = windowPosition' wleft  rleft

      stepper = defaultStepping device

      -- encode a single letter
      encode1  = encodeLetter' stepper device

      -- encode an entire message
      encode   = mapAccumL encode1

      package = EnigmaPackage device state stepper encode

  return package

parsePlugboard :: String -> Either String [(Char,Char)]
parsePlugboard str = do
  let str' = dropWhile (not.isLetter) str
  if null str'
    then return []
    else do (pair,rest) <- matchPair str'
            pairs <- parsePlugboard rest
            return (pair:pairs)
  where
    matchPair (a:b:rest) | isLetter a && isLetter b = return ((a,b),rest)
    matchPair _          = Left "unmatched letter in plugboard"

findRotor :: String -> Either String Rotor
findRotor "I"     = return rotorI
findRotor "II"    = return rotorII
findRotor "III"   = return rotorIII
findRotor "IV"    = return rotorIV
findRotor "V"     = return rotorV
findRotor "VI"    = return rotorVI
findRotor "VII"   = return rotorVII
findRotor "VIII"  = return rotorVIII
-- reflectors
findRotor "β"     = return $ makeRotor ukwBeta ""
findRotor "beta"  = return $ makeRotor ukwBeta ""
findRotor "γ"     = return $ makeRotor ukwGamma ""
findRotor "gamma" = return $ makeRotor ukwGamma ""
findRotor "A"     = return $ makeRotor ukwA ""
findRotor "B"     = return $ makeRotor ukwB ""
findRotor "C"     = return $ makeRotor ukwC ""
findRotor "BThin" = return $ makeRotor ukwBThin ""
findRotor "CThin" = return $ makeRotor ukwCThin ""
findRotor "b"     = return $ makeRotor ukwBThin ""
findRotor "c"     = return $ makeRotor ukwCThin ""
findRotor "id"    = return $ makeRotor letters  ""
findRotor ""      = return $ makeRotor letters  ""
findRotor name    = Left $ "unknown rotor name: " ++ name

