{-# LANGUAGE FlexibleContexts #-}

module Enigma.Device
  ( EnigmaDevice(..), encodeLetter, defaultStepping, encodeLetter'
  , mkEnigmaM3
  )
where

import Prelude hiding (log)
import Data.Char
import Data.List
import Enigma.Base
import Debug.Trace

import Control.Monad.State

data EnigmaDevice =
  EnigmaDevice {
    _stecker :: Cipher
  , _etw     :: Cipher
  , _right   :: Wheel
  , _middle  :: Wheel
  , _left    :: Wheel
  , _fourth  :: Wheel
  , _pos4    :: Position
  , _ukw     :: Permutation
  }
  deriving (Show,Eq)

-- default stepping
defaultStepping :: EnigmaDevice -> EnigmaState -> EnigmaState
defaultStepping e (EnigmaState r m l) = doubleStepping (_right e, r) (_middle e, m) (_left e, l)

-- default encoding procedure
encodeLetter :: EnigmaDevice -> EnigmaState -> Char -> (EnigmaState, Char)
encodeLetter e = encodeLetter' (defaultStepping e) e

lowerToUpper ch = chr( ord('A') + ord(ch) - ord('a') )
upperToLower ch = chr( ord('a') + ord(ch) - ord('A') )

-- Create a encoding function with a custom stepping function
encodeLetter' :: (EnigmaState -> EnigmaState) -> EnigmaDevice -> EnigmaState -> Char -> (EnigmaState, Char)
encodeLetter' stepping e state a =
  let state'@(EnigmaState r m l) = stepping state
      encode'' x = let pairs = [ (_stecker e,           0)
                                , (_etw e,              0)
                                , (_cipher (_right e),  r-1)
                                , (_cipher (_middle e), m-1)
                                , (_cipher (_left e),   l-1)
                                , (_cipher (_fourth e), (_pos4 e)-1)
                                ]
                   in conjugateEncode pairs (_ukw e) a
      -- the returned result
      (st, b) | 'A' <= a && a <= 'Z' = (state', encode'' a)
              | 'a' <= a && a <= 'z' = (state', upperToLower $ encode'' (lowerToUpper a))
              | otherwise            = (state, a)
  in  (st, b)

-- create a message encoder
mkEncoder :: (EnigmaDevice -> Stepper) -> EnigmaDevice -> MessageEncoder
mkEncoder stepping e = mapAccumL $ encodeLetter' (stepping e) e

mkEnigmaM3 :: Cipher         -- ^ stecker
           -> Wheel          -- ^ left wheel
           -> Wheel          -- ^ middle wheel
           -> Wheel          -- ^ right wheel
           -> Permutation    -- ^ reflector
           -> EnigmaDevice
mkEnigmaM3 stecker left middle right reflector
  = EnigmaDevice { _stecker = stecker
                 , _etw     = identityCipher
                 , _right   = right
                 , _middle  = middle
                 , _left    = left
                 , _fourth  = identityWheel
                 , _pos4    = 1
                 , _ukw     = reflector
                 }

