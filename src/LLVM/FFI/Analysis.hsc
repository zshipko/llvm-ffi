{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module LLVM.FFI.Analysis where

import LLVM.FFI.Core (ModuleRef, ValueRef)

import qualified Foreign.C.Types as C
import Foreign.C.String(CString)
import Foreign.Ptr(Ptr)

type CInt = C.CInt


type VerifierFailureAction = CInt

foreign import ccall unsafe "LLVMVerifyFunction" verifyFunction
    :: ValueRef -> VerifierFailureAction -> IO CInt
foreign import ccall unsafe "LLVMVerifyModule" verifyModule
    :: ModuleRef -> VerifierFailureAction -> (Ptr CString) -> IO CInt
foreign import ccall unsafe "LLVMViewFunctionCFG" viewFunctionCFG
    :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMViewFunctionCFGOnly" viewFunctionCFGOnly
    :: ValueRef -> IO ()
