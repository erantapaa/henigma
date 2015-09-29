module Enigma.Base where

import Data.Char
import Data.List
import Control.Applicative ((<|>))

letters = [ 'A' .. 'Z' ]
letterCount = length letters

letterOrd :: Char -> Int
letterOrd ch = ord ch - ord 'A'

letterChr :: Int -> Char
letterChr i = chr ( ord 'A' + (mod i letterCount) )

rotateLetter :: Int -> Char -> Char
rotateLetter i ch = letterChr (letterOrd ch + i) 

-- A Permutation should have exactly 26 letters and be a permutation of ['A'..'Z'].
type Permutation = String

identityPerm = letters

permEncode :: Permutation -> Char -> Char
permEncode perm ch = perm !! (letterOrd ch)

inversePerm :: Permutation -> Permutation
inversePerm perm = map snd $ sort [ (b,a) | a <- letters, let b = permEncode perm a ]

data Cipher = Cipher { _perm :: Permutation, _invperm :: Permutation }
       deriving (Show,Eq)

identityCipher = Cipher identityPerm identityPerm

makeCipher :: Permutation -> Cipher
makeCipher perm = Cipher perm (inversePerm perm)

-- Create a cipher from pairs
makeCipherFromPairs :: [(Char,Char)] -> Cipher
makeCipherFromPairs pairs = Cipher perm (inversePerm perm)
  where go a = let Just b = lookup a pairs <|> lookup a invPairs <|> (Just a)
               in b
        perm = map go letters
        invPairs = [ (b,a) | (a,b) <- pairs ]

permShiftEncode :: Permutation -> Int -> Char -> Char
permShiftEncode perm shift = rotateLetter (negate shift) . permEncode perm . rotateLetter shift

-- encode a letter through a chain of conjugations
conjugateEncode :: [(Cipher,Int)] -> Permutation -> Char -> Char
conjugateEncode [] perm = permEncode perm
conjugateEncode ((c,i):cs) perm = permShiftEncode (_invperm c) i
                                . conjugateEncode cs perm
                                . permShiftEncode (_perm c) i

data Wheel = Wheel { _cipher :: Cipher, _turnovers :: String, _ringSetting :: Int }
       deriving (Show,Eq)

-- Positions are 1-based and have the range 1..letterCount
type Position = Int

-- increment a position (which is 1-based, not 0-based)
incPosition :: Position -> Position
incPosition p = 1 + mod p letterCount

isTurnOver :: Wheel -> Position -> Bool
isTurnOver wheel pos = elem (windowLetter wheel pos) (_turnovers wheel)
  where window = elem (windowLetter wheel pos) (_turnovers wheel)
        -- note: -2 here because both pos and ring setting are 1 based

windowLetter :: Wheel -> Position -> Char
windowLetter wheel pos = letterChr (pos + _ringSetting wheel - 2)

-- return the position which places a letter in the window
windowPosition :: Wheel -> Char -> Position
windowPosition wheel a = 1 + mod (letterOrd a + 1 - _ringSetting wheel) letterCount

data EnigmaState = EnigmaState !Position !Position !Position -- right, middle, left
  deriving (Show,Eq)

type Stepping = (Wheel,Position) -> (Wheel,Position) -> (Wheel,Position) -> EnigmaState

-- gearbox stepping
gearboxStepping :: Stepping
gearboxStepping (rw,rp) (mw, mp) ( lw, lp) =
  let rp' = incPosition rp
      (mp',lp') | isTurnOver lw lp  = if isTurnOver mw mp
                                         then (incPosition mp, incPosition lp)
                                         else (incPosition mp, lp)
                | otherwise         = (mp,lp)
  in EnigmaState  rp' mp' lp'

-- normal stepping for three wheels
doubleStepping :: Stepping
doubleStepping (rw,rp) (mw, mp) (lw, lp) = EnigmaState  rp' mp' lp'
  where rp' = incPosition rp
        mp' = if (isTurnOver mw mp) || (isTurnOver rw rp)
               then incPosition mp
               else mp
        lp' = if isTurnOver mw mp
                then incPosition lp
                else lp

-- no stepping
noStepping :: Stepping
noStepping (_,rp) (_, mp) (_, lp) = EnigmaState  rp mp lp

-- encode an entire string using an encodeLetter function
encodeMessage :: (EnigmaState -> Char -> (EnigmaState, Char)) -> EnigmaState -> String -> (EnigmaState, String)
encodeMessage = mapAccumL

-- wheel definitions

makeWheel :: Rotor -> Int -> Wheel
makeWheel (perm,turns) ring = Wheel (makeCipher perm) turns ring

type Rotor = (String,String)

makeRotor a b = (a,b)

rotorI    = makeRotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Q"
rotorII   = makeRotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" "E"
rotorIII  = makeRotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" "V"
rotorIV   = makeRotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" "J"
rotorV    = makeRotor "VZBRGITYUPSDNHLXAWMJQOFECK" "Z"
rotorVI   = makeRotor "JPGVOUMFYQBENHZRDKASXLICTW" "ZM"
rotorVII  = makeRotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" "ZM"
rotorVIII = makeRotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" "ZM"

ukwBeta  = "LEYJVCNIXWPBQMDRTAKZGFUHOS"
ukwGamma = "FSOKANUERHMBTIYCWLQPZXVGJD"
ukwA     = "EJMZALYXVBWFCRQUONTSPIKHGD"
ukwB     = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
ukwC     = "FVPJIAOYEDRZXWGCTKUQSBNMHL"
ukwBThin = "ENKQAUYWJICOPBLMDXZVFTHRGS"
ukwCThin = "RDOBJNTKVEHMLFCWZAXGYIPSUQ"

etwQwerty   = makeCipher "QWERTZUIOASDFGHJKPYXCVBNML"
etwIdentity = identityCipher
etwKZR      = makeCipher "ILXRZTKGJYAMWVDUFCPQEONSHB"

identityWheel = Wheel identityCipher [] 0

type LetterEncoder  = EnigmaState -> Char -> (EnigmaState, Char)
type Stepper        = EnigmaState -> EnigmaState
type MessageEncoder = EnigmaState -> String -> (EnigmaState, String)
