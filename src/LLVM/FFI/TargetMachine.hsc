{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.TargetMachine where


import qualified LLVM.FFI.Core as LLVM
import LLVM.FFI.Target (TargetDataRef)
import LLVM.FFI.Core (ModuleRef, PassManagerRef, MemoryBufferRef)

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)
import Data.Word (Word32)


type CUInt = C.CUInt
type CInt = C.CInt
type Triple = CString
type ErrorMessage = CString


#include <llvm-c/TargetMachine.h>

newtype CodeGenOptLevel = CodeGenOptLevel (#type LLVMCodeGenOptLevel)
    deriving (Eq)

codeGenLevelNone, codeGenLevelLess :: CodeGenOptLevel
codeGenLevelDefault, codeGenLevelAggressive :: CodeGenOptLevel
codeGenLevelNone       = CodeGenOptLevel (#const LLVMCodeGenLevelNone)
codeGenLevelLess       = CodeGenOptLevel (#const LLVMCodeGenLevelLess)
codeGenLevelDefault    = CodeGenOptLevel (#const LLVMCodeGenLevelDefault)
codeGenLevelAggressive = CodeGenOptLevel (#const LLVMCodeGenLevelAggressive)

newtype RelocMode = RelocMode (#type LLVMRelocMode)
    deriving (Eq)

relocDefault      :: RelocMode
relocStatic       :: RelocMode
relocPIC          :: RelocMode
relocDynamicNoPic :: RelocMode
relocROPI         :: RelocMode
relocRWPI         :: RelocMode
relocROPI_RWPI    :: RelocMode

relocDefault      = RelocMode (#const LLVMRelocDefault)
relocStatic       = RelocMode (#const LLVMRelocStatic)
relocPIC          = RelocMode (#const LLVMRelocPIC)
relocDynamicNoPic = RelocMode (#const LLVMRelocDynamicNoPic)
relocROPI         = RelocMode (#const LLVMRelocROPI)
relocRWPI         = RelocMode (#const LLVMRelocRWPI)
relocROPI_RWPI    = RelocMode (#const LLVMRelocROPI_RWPI)

newtype CodeModel = CodeModel (#type LLVMCodeModel)
    deriving (Eq)

codeModelDefault    :: CodeModel
codeModelJITDefault :: CodeModel
codeModelTiny       :: CodeModel
codeModelSmall      :: CodeModel
codeModelKernel     :: CodeModel
codeModelMedium     :: CodeModel
codeModelLarge      :: CodeModel

codeModelDefault    = CodeModel (#const LLVMCodeModelDefault)
codeModelJITDefault = CodeModel (#const LLVMCodeModelJITDefault)
codeModelTiny       = CodeModel (#const LLVMCodeModelTiny)
codeModelSmall      = CodeModel (#const LLVMCodeModelSmall)
codeModelKernel     = CodeModel (#const LLVMCodeModelKernel)
codeModelMedium     = CodeModel (#const LLVMCodeModelMedium)
codeModelLarge      = CodeModel (#const LLVMCodeModelLarge)

newtype CodeGenFileType = CodeGenFileType (#type LLVMCodeGenFileType)
    deriving (Eq)

assemblyFile, objectFile :: CodeGenFileType
assemblyFile = CodeGenFileType (#const LLVMAssemblyFile)
objectFile   = CodeGenFileType (#const LLVMObjectFile)


data TargetMachine
    deriving (Typeable)
type TargetMachineRef = Ptr TargetMachine

data Target
    deriving (Typeable)
type TargetRef = Ptr Target


foreign import ccall unsafe "LLVMGetFirstTarget" getFirstTarget
    :: IO TargetRef

foreign import ccall unsafe "LLVMGetNextTarget" getNextTarget
    :: TargetRef -> IO TargetRef

foreign import ccall unsafe "LLVMGetTargetFromName" getTargetFromName
    :: CString -> IO TargetRef

foreign import ccall unsafe "LLVMGetTargetFromTriple" getTargetFromTriple
    :: Triple -> (Ptr TargetRef) -> (Ptr CString) -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetTargetName" getTargetName
    :: TargetRef -> IO CString

foreign import ccall unsafe "LLVMGetTargetDescription" getTargetDescription
    :: TargetRef -> IO CString

foreign import ccall unsafe "LLVMTargetHasJIT" targetHasJIT
    :: TargetRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMTargetHasTargetMachine" targetHasTargetMachine
    :: TargetRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMTargetHasAsmBackend" targetHasAsmBackend
    :: TargetRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMCreateTargetMachine" createTargetMachine
    :: TargetRef -> CString -> CString -> CString -> CodeGenOptLevel -> RelocMode -> CodeModel -> IO TargetMachineRef

foreign import ccall unsafe "LLVMDisposeTargetMachine" disposeTargetMachine
    :: TargetMachineRef -> IO ()

foreign import ccall unsafe "LLVMGetTargetMachineTarget" getTargetMachineTarget
    :: TargetMachineRef -> IO TargetRef

foreign import ccall unsafe "LLVMGetTargetMachineTriple" getTargetMachineTriple
    :: TargetMachineRef -> IO Triple

foreign import ccall unsafe "LLVMGetTargetMachineCPU" getTargetMachineCPU
    :: TargetMachineRef -> IO CString

foreign import ccall unsafe "LLVMGetTargetMachineFeatureString" getTargetMachineFeatureString
    :: TargetMachineRef -> IO CString

foreign import ccall unsafe "LLVMCreateTargetDataLayout" createTargetDataLayout
    :: TargetMachineRef -> IO TargetDataRef

foreign import ccall unsafe "LLVMSetTargetMachineAsmVerbosity" setTargetMachineAsmVerbosity
    :: TargetMachineRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMTargetMachineEmitToFile" targetMachineEmitToFile
    :: TargetMachineRef -> ModuleRef -> CString -> CodeGenFileType -> Ptr ErrorMessage -> IO LLVM.Bool

foreign import ccall unsafe "LLVMTargetMachineEmitToMemoryBuffer" targetMachineEmitToMemoryBuffer
    :: TargetMachineRef -> ModuleRef -> CodeGenFileType -> Ptr ErrorMessage -> (Ptr MemoryBufferRef) -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetDefaultTargetTriple" getDefaultTargetTriple
    :: IO Triple

foreign import ccall unsafe "LLVMNormalizeTargetTriple" normalizeTargetTriple
    :: Triple -> IO CString

foreign import ccall unsafe "LLVMGetHostCPUName" getHostCPUName
    :: IO CString

foreign import ccall unsafe "LLVMGetHostCPUFeatures" getHostCPUFeatures
    :: IO CString

foreign import ccall unsafe "LLVMAddAnalysisPasses" addAnalysisPasses
    :: TargetMachineRef -> PassManagerRef -> IO ()
