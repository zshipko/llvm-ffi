{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.Target where

import LLVM.FFI.Core (ValueRef, TypeRef, PassManagerRef)

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)


type CUInt = C.CUInt
type CInt = C.CInt
type CULLong = C.CULLong


-- enum { LLVMBigEndian, LLVMLittleEndian };
type ByteOrdering = CInt

data TargetData
    deriving (Typeable)
type TargetDataRef = Ptr TargetData

data TargetLibraryInfo
    deriving (Typeable)
type TargetLibraryInfoRef = Ptr TargetLibraryInfo

foreign import ccall unsafe "LLVMCreateTargetData" createTargetData
    :: CString -> IO TargetDataRef
foreign import ccall unsafe "LLVMAddTargetData" addTargetData
    :: TargetDataRef -> PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddTargetLibraryInfo" addTargetLibraryInfo
    :: TargetLibraryInfoRef -> PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMCopyStringRepOfTargetData" copyStringRepOfTargetData
    :: TargetDataRef -> IO CString
foreign import ccall unsafe "LLVMByteOrder" byteOrder
    :: TargetDataRef -> IO ByteOrdering
foreign import ccall unsafe "LLVMPointerSize" pointerSize
    :: TargetDataRef -> IO CUInt
foreign import ccall unsafe "LLVMIntPtrType" intPtrType
    :: TargetDataRef -> IO TypeRef
foreign import ccall unsafe "LLVMSizeOfTypeInBits" sizeOfTypeInBits
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMStoreSizeOfType" storeSizeOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMABISizeOfType" aBISizeOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMABIAlignmentOfType" aBIAlignmentOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMCallFrameAlignmentOfType" callFrameAlignmentOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMPreferredAlignmentOfType" preferredAlignmentOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMPreferredAlignmentOfGlobal" preferredAlignmentOfGlobal
    :: TargetDataRef -> ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMElementAtOffset" elementAtOffset
    :: TargetDataRef -> TypeRef -> CULLong -> IO CUInt
foreign import ccall unsafe "LLVMOffsetOfElement" offsetOfElement
    :: TargetDataRef -> TypeRef -> CUInt -> IO CULLong
foreign import ccall unsafe "LLVMDisposeTargetData" disposeTargetData
    :: TargetDataRef -> IO ()
