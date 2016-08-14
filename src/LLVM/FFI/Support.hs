{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module LLVM.FFI.Support
    (
      createStandardModulePasses
    , createStandardFunctionPasses
    ) where

import qualified Foreign.C.Types as C

import LLVM.FFI.Core (PassManagerRef)


type CUInt = C.CUInt
type CInt = C.CInt


foreign import ccall unsafe "LLVMCreateStandardFunctionPasses" createStandardFunctionPasses
    :: PassManagerRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMCreateStandardModulePasses" createStandardModulePasses
    :: PassManagerRef -> CUInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
