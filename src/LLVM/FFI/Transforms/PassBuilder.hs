{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.Transforms.PassBuilder where

import qualified LLVM.FFI.Core as LLVM
import LLVM.FFI.TargetMachine (TargetMachineRef)
import LLVM.FFI.Error (ErrorRef)
import LLVM.FFI.Core (ModuleRef)

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)


type CUInt = C.CUInt
type CInt = C.CInt


data PassBuilderOptions
    deriving (Typeable)
type PassBuilderOptionsRef = Ptr PassBuilderOptions


foreign import ccall unsafe "LLVMRunPasses" runPasses
    :: ModuleRef -> CString -> TargetMachineRef -> PassBuilderOptionsRef -> IO ErrorRef

foreign import ccall unsafe "LLVMCreatePassBuilderOptions" createPassBuilderOptions
    :: IO PassBuilderOptionsRef

foreign import ccall unsafe "LLVMPassBuilderOptionsSetVerifyEach" passBuilderOptionsSetVerifyEach
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetDebugLogging" passBuilderOptionsSetDebugLogging
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetLoopInterleaving" passBuilderOptionsSetLoopInterleaving
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetLoopVectorization" passBuilderOptionsSetLoopVectorization
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetSLPVectorization" passBuilderOptionsSetSLPVectorization
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetLoopUnrolling" passBuilderOptionsSetLoopUnrolling
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll" passBuilderOptionsSetForgetAllSCEVInLoopUnroll
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetLicmMssaOptCap" passBuilderOptionsSetLicmMssaOptCap
    :: PassBuilderOptionsRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap" passBuilderOptionsSetLicmMssaNoAccForPromotionCap
    :: PassBuilderOptionsRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetCallGraphProfile" passBuilderOptionsSetCallGraphProfile
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetMergeFunctions" passBuilderOptionsSetMergeFunctions
    :: PassBuilderOptionsRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassBuilderOptionsSetInlinerThreshold" passBuilderOptionsSetInlinerThreshold
    :: PassBuilderOptionsRef -> CInt -> IO ()

foreign import ccall unsafe "LLVMDisposePassBuilderOptions" disposePassBuilderOptions
    :: PassBuilderOptionsRef -> IO ()
