{-# LANGUAGE FlexibleContexts #-}

module Enigma.Device
  ( EnigmaDevice(..), encodeLetter, defaultStepping, encodeLetter'
  , mkEnigmaM3
  )
where

import Prelude hiding (log)
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

-- Create a encoding function with a custom stepping function
encodeLetter' :: (EnigmaState -> EnigmaState) -> EnigmaDevice -> EnigmaState -> Char -> (EnigmaState, Char)
encodeLetter' stepping e state a =
  let state'@(EnigmaState r m l) = stepping state
      pairs           = [ (_stecker e,          0)
                        , (_etw e,              0)
                        , (_cipher (_right e),  r-1)
                        , (_cipher (_middle e), m-1)
                        , (_cipher (_left e),   l-1)
                        , (_cipher (_fourth e), (_pos4 e)-1)
                        ]
      b = conjugateEncode pairs (_ukw e) a
  in ( state', b )

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

