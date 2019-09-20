{-# LANGUAGE Safe #-}
module LLVM.FFI.Base where

import qualified Data.Bool as Bool
import Data.Int (Int32)

import Prelude
         (Eq, Enum, Show, fromIntegral, show, fromEnum, toEnum, (.), (==))

#include <llvm-c/Core.h>


newtype Bool = Bool (#type LLVMBool)
    deriving (Eq)

instance Enum Bool where
    fromEnum (Bool b) = fromIntegral b
    toEnum = Bool . fromIntegral

instance Show Bool where
    show b = if b == false then "false" else "true"

false, true :: Bool
false = Bool 0; true = Bool 1

consBool :: Bool.Bool -> Bool
consBool = toEnum . fromEnum

deconsBool :: Bool -> Bool.Bool
deconsBool = toEnum . fromEnum
