{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLVM.Raw.Core where

import qualified LLVM.FFI.Base as LLVM

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, FunPtr)

import Data.Typeable (Typeable)

import Data.Word (Word8, Word32, Word64)


type CDouble  = C.CDouble
type CInt     = C.CInt
type CUInt    = C.CUInt
type CLLong   = C.CLLong
type CULLong  = C.CULLong
type CSize    = C.CSize


#include <llvm/Config/llvm-config.h>
#include <llvm-c/Core.h>


data Module
    deriving (Typeable)
type ModuleRef = Ptr Module

data ModuleProvider
    deriving (Typeable)
type ModuleProviderRef = Ptr ModuleProvider

data Type
    deriving (Typeable)
type TypeRef = Ptr Type

data BasicBlock
    deriving (Typeable)
type BasicBlockRef = Ptr BasicBlock

data Value
    deriving (Typeable)
type ValueRef = Ptr Value

data OpaqueUse
    deriving (Typeable)
type UseRef = Ptr OpaqueUse

data Builder
    deriving (Typeable)
type BuilderRef = Ptr Builder

data MemoryBuffer
    deriving (Typeable)
type MemoryBufferRef = Ptr MemoryBuffer

data PassManager
    deriving (Typeable)
type PassManagerRef = Ptr PassManager

data PassRegistry
    deriving (Typeable)
type PassRegistryRef = Ptr PassRegistry

data Context
    deriving (Typeable)
type ContextRef = Ptr Context

{-
data Attribute
    deriving (Typeable)
-}
-- until 3.9
newtype Attribute = Attribute Word32
type AttributeRef = Ptr Attribute

newtype AttributeIndex = AttributeIndex #{type LLVMAttributeIndex}

attributeReturnIndex, attributeFunctionIndex :: AttributeIndex
attributeReturnIndex   = AttributeIndex (#const LLVMAttributeReturnIndex)
attributeFunctionIndex = AttributeIndex (#const LLVMAttributeFunctionIndex)


data Metadata
    deriving (Typeable)
type MetadataRef = Ptr Metadata

data DiagnosticInfo
    deriving (Typeable)
type DiagnosticInfoRef = Ptr DiagnosticInfo

data NamedMDNode
    deriving (Typeable)
type NamedMDNodeRef = Ptr NamedMDNode


data ModuleFlagEntry
    deriving (Typeable)
data ValueMetadataEntry
    deriving (Typeable)

type DiagnosticHandler = FunPtr (DiagnosticInfoRef -> Ptr () -> IO ())
type YieldCallback = FunPtr (ContextRef -> Ptr () -> IO ())

newtype CallingConvention = CallingConvention {unCallingConvention :: CUInt}

newtype AtomicOrdering = AtomicOrdering #{type LLVMAtomicOrdering}
newtype AtomicRMWBinOp = AtomicRMWBinOp #{type LLVMAtomicRMWBinOp}
newtype DiagnosticSeverity = DiagnosticSeverity #{type LLVMDiagnosticSeverity}
newtype DLLStorageClass = DLLStorageClass #{type LLVMDLLStorageClass}
-- newtype InlineAsmDialect = InlineAsmDialect #{type LLVMInlineAsmDialect}
newtype InlineAsmDialect = InlineAsmDialect Word32
newtype IntPredicate = IntPredicate #{type LLVMIntPredicate}
newtype Linkage = Linkage #{type LLVMLinkage}
-- newtype ModuleFlagBehavior = ModuleFlagBehavior #{type LLVMModuleFlagBehavior}
newtype ModuleFlagBehavior = ModuleFlagBehavior Word32
newtype Opcode = Opcode #{type LLVMOpcode}
newtype RealPredicate = RealPredicate #{type LLVMRealPredicate}
newtype ThreadLocalMode = ThreadLocalMode #{type LLVMThreadLocalMode}
newtype TypeKind = TypeKind {unTypeKind :: #{type LLVMTypeKind}}
-- newtype UnnamedAddr = UnnamedAddr #{type LLVMUnnamedAddr}
newtype UnnamedAddr = UnnamedAddr Word32
newtype ValueKind = ValueKind #{type LLVMValueKind}
newtype Visibility = Visibility #{type LLVMVisibility}


