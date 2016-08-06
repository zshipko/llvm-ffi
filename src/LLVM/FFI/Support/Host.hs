{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module LLVM.FFI.Support.Host (
    getHostCPUName,
    ) where

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "LLVMGetHostCPUName" getHostCPUName
    :: Ptr C.CSize -> IO CString
