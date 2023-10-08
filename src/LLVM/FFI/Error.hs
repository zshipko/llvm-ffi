{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.Error where

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)


type CUInt = C.CUInt
type CInt = C.CInt


data Error
    deriving (Typeable)
type ErrorRef = Ptr Error

data ErrorTypeIdObj
    deriving (Typeable)
type ErrorTypeId = Ptr ErrorTypeIdObj


foreign import ccall unsafe "LLVMGetErrorTypeId" getErrorTypeId
    :: ErrorRef -> IO ErrorTypeId

foreign import ccall unsafe "LLVMConsumeError" consumeError
    :: ErrorRef -> IO ()

foreign import ccall unsafe "LLVMGetErrorMessage" getErrorMessage
    :: ErrorRef -> IO CString

foreign import ccall unsafe "LLVMDisposeErrorMessage" disposeErrorMessage
    :: CString -> IO ()

foreign import ccall unsafe "LLVMGetStringErrorTypeId" getStringErrorTypeId
    :: IO ErrorTypeId

foreign import ccall unsafe "LLVMCreateStringError" createStringError
    :: CString -> IO ErrorRef
