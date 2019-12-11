{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module:      LLVM.FFI.Core
-- Copyright:   Bryan O'Sullivan 2007, 2008
-- License:     BSD-style (see the file LICENSE)
--
-- Maintainer:  bos@serpentine.com
-- Stability:   experimental
-- Portability: requires GHC 6.8, LLVM
--
-- This module provides direct access to the LLVM C bindings.

module LLVM.FFI.Core
    (
      initializeCore

    -- * Boolean values
    , LLVM.Bool(LLVM.Bool)
    , LLVM.false
    , LLVM.true
    , LLVM.consBool
    , LLVM.deconsBool

    -- * Error handling
    , disposeMessage

    -- * Context functions
    , Context
    , ContextRef
    , contextCreate
    , contextDispose
    , getGlobalContext

    , getMDKindID
    , getMDKindIDInContext

      -- * Modules
    , Module
    , ModuleRef
    , moduleCreateWithName
    , moduleCreateWithNameInContext
    , disposeModule
    , ptrDisposeModule

    , getDataLayout
    , setDataLayout

    , getTarget
    , setTarget

    , defaultTargetTriple
    , hostTriple

    , dumpModule

    , setModuleInlineAsm
    , getModuleContext

    -- * Types
    , Type
    , TypeRef
    , TypeKind(..)

    , getTypeKind
    , typeIsSized
    , getTypeContext

    -- ** Integer types
    , int1TypeInContext
    , int8TypeInContext
    , int16TypeInContext
    , int32TypeInContext
    , int64TypeInContext
    , intTypeInContext

    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType
    , getIntTypeWidth

    -- ** Real types
    , floatTypeInContext
    , doubleTypeInContext
    , x86FP80TypeInContext
    , fp128TypeInContext
    , ppcFP128TypeInContext

    , floatType
    , doubleType
    , x86FP80Type
    , fp128Type
    , ppcFP128Type

    -- ** Function types
    , functionType
    , isFunctionVarArg
    , getReturnType
    , countParamTypes
    , getParamTypes

    -- ** Struct types
    , structTypeInContext
    , structType
    , structCreateNamed
    , getStructName
    , structSetBody

    , countStructElementTypes
    , getStructElementTypes
    , isPackedStruct
    , isOpaqueStruct

    , getTypeByName

    -- ** Array, pointer, and vector types
    , arrayType
    , pointerType
    , vectorType

    , getElementType
    , getArrayLength
    , getPointerAddressSpace
    , getVectorSize

    -- ** Other types
    , voidTypeInContext
    , labelTypeInContext
    , x86MMXTypeInContext

    , voidType
    , labelType
    , x86MMXType

    -- * Values
    , Value
    , ValueRef
    , typeOf
    , getValueName
    , setValueName
    , dumpValue
    , replaceAllUsesWith
    , hasMetadata
    , getMetadata
    , setMetadata

    -- ** Uses
    , OpaqueUse
    , UseRef
    , getFirstUse
    , getNextUse
    , getUser
    , getUsedValue

    -- ** Users
    , getOperand
    , setOperand
    , getNumOperands

    -- ** Constants
    , constNull
    , constAllOnes
    , getUndef
    , isConstant
    , isNull
    , isUndef
    , constPointerNull

    -- ** Metadata
    , mDStringInContext
    , mDString
    , mDNodeInContext
    , mDNode
    , getMDString
--    , getMDNodeNumOperands
--    , getMDNodeOperand
    , getNamedMetadataNumOperands
    , getNamedMetadataOperands

    -- ** Scalar constants
    , constInt
    , constIntOfArbitraryPrecision
    , constIntOfString
    , constIntOfStringAndSize
    , constReal
    , constRealOfString
    , constRealOfStringAndSize
    , constIntGetZExtValue
    , constIntGetSExtValue

    -- ** Composite constants
    , constStringInContext
    , constStructInContext
    , constString
    , constArray
    , constStruct
    , constNamedStruct
    , constVector

    -- ** Constant Expressions
    , getConstOpcode
    , alignOf
    , sizeOf
    , constNeg
    , constNUWNeg
    , constNSWNeg
    , constFNeg
    , constNot
    , constAdd
    , constNSWAdd
    , constNUWAdd
    , constFAdd
    , constSub
    , constNSWSub
    , constNUWSub
    , constFSub
    , constMul
    , constNSWMul
    , constNUWMul
    , constFMul
    , constUDiv
    , constSDiv
    , constExactSDiv
    , constFDiv
    , constURem
    , constSRem
    , constFRem
    , constAnd
    , constOr
    , constXor
    , constICmp
    , constFCmp
    , constShl
    , constLShr
    , constAShr
    , constGEP
    , constInBoundsGEP
    , constTrunc
    , constSExt
    , constZExt
    , constFPTrunc
    , constFPExt
    , constUIToFP
    , constSIToFP
    , constFPToUI
    , constFPToSI
    , constPtrToInt
    , constIntToPtr
    , constBitCast
    , constZExtOrBitCast
    , constSExtOrBitCast
    , constTruncOrBitCast
    , constPointerCast
    , constIntCast
    , constFPCast
    , constSelect
    , constExtractElement
    , constInsertElement
    , constShuffleVector
    , constExtractValue
    , constInsertValue
    , constInlineAsm
    , blockAddress

    -- ** Floating point attributes
    , setFastMath
    , setHasUnsafeAlgebra
    , setHasNoNaNs
    , setHasNoInfs
    , setHasNoSignedZeros
    , setHasAllowReciprocal
    , setHasAllowReassoc
    , setHasApproxFunc

    -- ** Support operations and types
    , Linkage(..)
    , fromLinkage
    , toLinkage

    , Visibility(..)
    , fromVisibility
    , toVisibility

    -- ** Global variables, functions, and aliases (globals)
    , getGlobalParent
    , isDeclaration
    , getLinkage
    , setLinkage
    , getSection
    , setSection
    , getVisibility
    , setVisibility
    , getAlignment
    , setAlignment

    -- ** Global variables
    , addGlobal
    , addGlobalInAddressSpace
    , getNamedGlobal
    , getFirstGlobal
    , getLastGlobal
    , getNextGlobal
    , getPreviousGlobal
    , deleteGlobal
    , getInitializer
    , setInitializer
    , isThreadLocal
    , setThreadLocal
    , isGlobalConstant
    , setGlobalConstant

    -- ** Aliases
    , addAlias

    -- * Parameter passing
    , Attribute
    , AttributeKind(..)

    -- ** Calling conventions
    , CallingConvention(..)
    , fromCallingConvention
    , toCallingConvention

    -- ** Functions
    , addFunction
    , getNamedFunction
    , getFirstFunction
    , getLastFunction
    , getNextFunction
    , getPreviousFunction
    , deleteFunction
    , getIntrinsicID
    , getFunctionCallConv
    , setFunctionCallConv
    , getGC
    , setGC
    , AttributeIndex(AttributeIndex)
    , attributeReturnIndex, attributeFunctionIndex
    , getEnumAttributeKindForName
    , getLastEnumAttributeKind
    , createEnumAttribute
    , getEnumAttributeKind
    , getEnumAttributeValue
    , createStringAttribute
    , getStringAttributeKind
    , getStringAttributeValue
    , isEnumAttribute
    , isStringAttribute
    , addAttributeAtIndex
    , getAttributeCountAtIndex
    , getAttributesAtIndex
    , getEnumAttributeAtIndex
    , getStringAttributeAtIndex
    , removeEnumAttributeAtIndex
    , removeStringAttributeAtIndex
    , addTargetDependentFunctionAttr
    , addCallSiteAttribute
    , getCallSiteAttributeCount
    , getCallSiteAttributes
    , getCallSiteEnumAttribute
    , getCallSiteStringAttribute
    , removeCallSiteEnumAttribute
    , removeCallSiteStringAttribute
    , getCalledValue

    -- ** Parameters
    , countParams
    , getParams
    , getParam
    , getParamParent
    , getFirstParam
    , getLastParam
    , getNextParam
    , getPreviousParam
    , setParamAlignment

    -- ** Basic blocks
    , BasicBlock
    , BasicBlockRef
    , basicBlockAsValue
    , valueIsBasicBlock
    , valueAsBasicBlock
    , getBasicBlockParent
    , getBasicBlockTerminator
    , countBasicBlocks
    , getBasicBlocks
    , getFirstBasicBlock
    , getLastBasicBlock
    , getNextBasicBlock
    , getPreviousBasicBlock
    , getEntryBasicBlock
    , appendBasicBlockInContext
    , insertBasicBlockInContext
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock
    , removeBasicBlockFromParent
    , moveBasicBlockBefore
    , moveBasicBlockAfter
    , getFirstInstruction
    , getLastInstruction

    -- ** Instructions
    , getInstructionParent
    , getNextInstruction
    , getPreviousInstruction
    , instructionEraseFromParent
    , getInstructionOpcode
    , getICmpPredicate

    -- ** Call Sites
    , getInstructionCallConv
    , setInstructionCallConv
    , setInstrParamAlignment

    -- ** Call Instructions (only)
    , isTailCall
    , setTailCall

    -- ** Switch Instructions (only)
    , getSwitchDefaultDest

    -- ** Phi nodes
    , addIncoming
    , countIncoming
    , getIncomingValue
    , getIncomingBlock

    -- * Instruction building
    , Builder
    , BuilderRef
    , createBuilderInContext
    , createBuilder
    , positionBuilder
    , positionBefore
    , positionAtEnd
    , getInsertBlock
    , clearInsertionPosition
    , insertIntoBuilder
    , insertIntoBuilderWithName
    , ptrDisposeBuilder

    -- ** Metadata
    , setCurrentDebugLocation
    , getCurrentDebugLocation
    , setInstDebugLocation

    -- ** Terminators
    , buildRetVoid
    , buildRet
    , buildAggregateRet
    , buildBr
    , buildCondBr
    , buildSwitch
    , buildIndirectBr
    , buildInvoke
    , buildLandingPad
    , buildResume
    , buildUnreachable

    , addCase
    , addDestination
    , addClause
    , setCleanup

    -- ** Arithmetic
    , buildAdd
    , buildNSWAdd
    , buildNUWAdd
    , buildFAdd
    , buildSub
    , buildNSWSub
    , buildNUWSub
    , buildFSub
    , buildMul
    , buildNSWMul
    , buildNUWMul
    , buildFMul
    , buildUDiv
    , buildSDiv
    , buildExactSDiv
    , buildFDiv
    , buildURem
    , buildSRem
    , buildFRem
    , buildShl
    , buildLShr
    , buildAShr
    , buildAnd
    , buildOr
    , buildXor
    , buildBinOp
    , buildNeg
    , buildNSWNeg
    , buildNUWNeg
    , buildFNeg
    , buildNot

    -- ** Memory
    , buildMalloc
    , buildArrayMalloc
    , buildAlloca
    , buildArrayAlloca
    , buildFree
    , buildLoad
    , buildStore
    , buildGEP
    , buildInBoundsGEP
    , buildStructGEP
    , buildGlobalString
    , buildGlobalStringPtr

    -- ** Casts
    , buildTrunc
    , buildZExt
    , buildSExt
    , buildFPToUI
    , buildFPToSI
    , buildUIToFP
    , buildSIToFP
    , buildFPTrunc
    , buildFPExt
    , buildPtrToInt
    , buildIntToPtr
    , buildBitCast
    , buildZExtOrBitCast
    , buildSExtOrBitCast
    , buildTruncOrBitCast
    , buildCast
    , buildPointerCast
    , buildIntCast
    , buildFPCast

    -- ** Comparisons
    , buildICmp
    , buildFCmp

    -- ** Miscellaneous instructions
    , buildPhi
    , buildCall
    , buildSelect
    , buildVAArg
    , buildExtractElement
    , buildInsertElement
    , buildShuffleVector
    , buildExtractValue
    , buildInsertValue
    , buildIsNull
    , buildIsNotNull
    , buildPtrDiff

    -- * Memory buffers
    , MemoryBuffer
    , MemoryBufferRef
    , createMemoryBufferWithContentsOfFile
    , createMemoryBufferWithSTDIN
    , disposeMemoryBuffer

    -- ** PassRegistry
    , PassRegistry
    , PassRegistryRef
    , getGlobalPassRegistry

    -- ** Pass manager
    , PassManager
    , PassManagerRef
    , ptrDisposePassManager

    , createPassManager
    , createFunctionPassManagerForModule
    , runPassManager
    , initializeFunctionPassManager
    , runFunctionPassManager
    , finalizeFunctionPassManager
    , disposePassManager

    -- ** Functions from extras.cpp
    , getNumUses
    , instGetOpcode
    , cmpInstGetPredicate

    ) where

import qualified LLVM.FFI.Base as LLVM

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, FunPtr)

import Data.Typeable (Typeable)

import Data.Word (Word32, Word64)

import Prelude
         (IO, Eq, Ord, Int, Bounded, Enum, Show, Read, String,
          ($), (++), (.), error,
           fmap, fromIntegral, show, toEnum, )


type CDouble  = C.CDouble
type CInt     = C.CInt
type CUInt    = C.CUInt
type CLLong   = C.CLLong
type CULLong  = C.CULLong


#include <llvm/Config/llvm-config.h>
#include <llvm-c/Core.h>


data Module
    deriving (Typeable)
type ModuleRef = Ptr Module

data Type
    deriving (Typeable)
type TypeRef = Ptr Type

type BasicBlock = Value
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

data Attribute
    deriving (Typeable)
type AttributeRef = Ptr Attribute

newtype AttributeIndex = AttributeIndex (#type LLVMAttributeIndex)

attributeReturnIndex, attributeFunctionIndex :: AttributeIndex
attributeReturnIndex   = AttributeIndex (#const LLVMAttributeReturnIndex)
attributeFunctionIndex = AttributeIndex (#const LLVMAttributeFunctionIndex)


defaultTargetTriple, hostTriple :: String
defaultTargetTriple = (#const_str LLVM_DEFAULT_TARGET_TRIPLE)
hostTriple          = (#const_str LLVM_HOST_TRIPLE)


data TypeKind
    = VoidTypeKind
    | FloatTypeKind
    | DoubleTypeKind
    | X86_FP80TypeKind
    | FP128TypeKind
    | PPC_FP128TypeKind
    | LabelTypeKind
    | IntegerTypeKind
    | FunctionTypeKind
    | StructTypeKind
    | ArrayTypeKind
    | PointerTypeKind
    | OpaqueTypeKind
    | VectorTypeKind
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable)

getTypeKind :: TypeRef -> IO TypeKind
getTypeKind = fmap (toEnum . fromIntegral) . getTypeKindCUInt

data CallingConvention = C
                       | Fast
                       | Cold
                       | X86StdCall
                       | X86FastCall
                       | GHC
                         deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

fromCallingConvention :: CallingConvention -> CUInt
fromCallingConvention c =
    case c of
        C -> (#const LLVMCCallConv)
        Fast -> (#const LLVMFastCallConv)
        Cold -> (#const LLVMColdCallConv)
        X86StdCall -> (#const LLVMX86FastcallCallConv)
        X86FastCall -> (#const LLVMX86StdcallCallConv)
        GHC -> 10

toCallingConvention :: CUInt -> CallingConvention
toCallingConvention c =
    case c of
        (#const LLVMCCallConv) -> C
        (#const LLVMFastCallConv) -> Fast
        (#const LLVMColdCallConv) -> Cold
        (#const LLVMX86StdcallCallConv) -> X86StdCall
        (#const LLVMX86FastcallCallConv) -> X86FastCall
        10 -> GHC
        _ ->
            error $ "LLVM.Core.FFI.toCallingConvention: " ++
                                "unsupported calling convention" ++ show c

-- |An enumeration for the kinds of linkage for global values.
data Linkage
    = ExternalLinkage     -- ^Externally visible function
    | AvailableExternallyLinkage
    | LinkOnceAnyLinkage  -- ^Keep one copy of function when linking (inline)
    | LinkOnceODRLinkage  -- ^Same, but only replaced by something equivalent.
    | LinkOnceODRAutoHideLinkage -- ^Like LinkOnceODR, but possibly hidden.
    | WeakAnyLinkage      -- ^Keep one copy of named function when linking (weak)
    | WeakODRLinkage      -- ^Same, but only replaced by something equivalent.
    | AppendingLinkage    -- ^Special purpose, only applies to global arrays
    | InternalLinkage     -- ^Rename collisions when linking (static functions)
    | PrivateLinkage      -- ^Like Internal, but omit from symbol table
    | DLLImportLinkage    -- ^Function to be imported from DLL
    | DLLExportLinkage    -- ^Function to be accessible from DLL
    | ExternalWeakLinkage -- ^ExternalWeak linkage description
    | GhostLinkage        -- ^Stand-in functions for streaming fns from BC files
    | CommonLinkage       -- ^Tentative definitions
    | LinkerPrivateLinkage -- ^Like Private, but linker removes.
    | LinkerPrivateWeakLinkage -- ^Like LinkerPrivate, but is weak.
    deriving (Show, Eq, Ord, Enum, Typeable)

fromLinkage :: Linkage -> CUInt
fromLinkage c =
    case c of
        ExternalLinkage             -> (#const LLVMExternalLinkage)
        AvailableExternallyLinkage  -> (#const LLVMAvailableExternallyLinkage)
        LinkOnceAnyLinkage          -> (#const LLVMLinkOnceAnyLinkage)
        LinkOnceODRLinkage          -> (#const LLVMLinkOnceODRLinkage)
        LinkOnceODRAutoHideLinkage  -> (#const LLVMLinkOnceODRAutoHideLinkage)
        WeakAnyLinkage              -> (#const LLVMWeakAnyLinkage)
        WeakODRLinkage              -> (#const LLVMWeakODRLinkage)
        AppendingLinkage            -> (#const LLVMAppendingLinkage)
        InternalLinkage             -> (#const LLVMInternalLinkage)
        PrivateLinkage              -> (#const LLVMPrivateLinkage)
        DLLImportLinkage            -> (#const LLVMDLLImportLinkage)
        DLLExportLinkage            -> (#const LLVMDLLExportLinkage)
        ExternalWeakLinkage         -> (#const LLVMExternalWeakLinkage)
        GhostLinkage                -> (#const LLVMGhostLinkage)
        CommonLinkage               -> (#const LLVMCommonLinkage)
        LinkerPrivateLinkage        -> (#const LLVMLinkerPrivateLinkage)
        LinkerPrivateWeakLinkage    -> (#const LLVMLinkerPrivateWeakLinkage)

toLinkage :: CUInt -> Linkage
toLinkage c =
    case c of
        (#const LLVMExternalLinkage)             -> ExternalLinkage
        (#const LLVMAvailableExternallyLinkage)  -> AvailableExternallyLinkage
        (#const LLVMLinkOnceAnyLinkage)          -> LinkOnceAnyLinkage
        (#const LLVMLinkOnceODRLinkage)          -> LinkOnceODRLinkage
        (#const LLVMLinkOnceODRAutoHideLinkage)  -> LinkOnceODRAutoHideLinkage
        (#const LLVMWeakAnyLinkage)              -> WeakAnyLinkage
        (#const LLVMWeakODRLinkage)              -> WeakODRLinkage
        (#const LLVMAppendingLinkage)            -> AppendingLinkage
        (#const LLVMInternalLinkage)             -> InternalLinkage
        (#const LLVMPrivateLinkage)              -> PrivateLinkage
        (#const LLVMDLLImportLinkage)            -> DLLImportLinkage
        (#const LLVMDLLExportLinkage)            -> DLLExportLinkage
        (#const LLVMExternalWeakLinkage)         -> ExternalWeakLinkage
        (#const LLVMGhostLinkage)                -> GhostLinkage
        (#const LLVMCommonLinkage)               -> CommonLinkage
        (#const LLVMLinkerPrivateLinkage)        -> LinkerPrivateLinkage
        (#const LLVMLinkerPrivateWeakLinkage)    -> LinkerPrivateWeakLinkage
        _ -> error "toLinkage: bad value"

-- |An enumeration for the kinds of visibility of global values.
data Visibility
    = DefaultVisibility   -- ^The GV is visible
    | HiddenVisibility    -- ^The GV is hidden
    | ProtectedVisibility -- ^The GV is protected
    deriving (Show, Eq, Ord, Enum)

fromVisibility :: Visibility -> CUInt
fromVisibility c =
    case c of
        DefaultVisibility   -> (#const LLVMDefaultVisibility)
        HiddenVisibility    -> (#const LLVMHiddenVisibility)
        ProtectedVisibility -> (#const LLVMProtectedVisibility)

toVisibility :: CUInt -> Visibility
toVisibility c =
    case c of
        (#const LLVMDefaultVisibility)   -> DefaultVisibility
        (#const LLVMHiddenVisibility)    -> HiddenVisibility
        (#const LLVMProtectedVisibility) -> ProtectedVisibility
        _ -> error "toVisibility: bad value"

newtype AttributeKind = AttributeKind CUInt

-- ** Initialization
foreign import ccall unsafe "LLVMInitializeCore" initializeCore
    :: PassRegistryRef -> IO ()

-- ** Error Handling
foreign import ccall unsafe "LLVMDisposeMessage" disposeMessage
    :: CString -> IO ()

-- ** Contexts
foreign import ccall unsafe "LLVMContextCreate" contextCreate
    :: IO ContextRef
foreign import ccall unsafe "LLVMGetGlobalContext" getGlobalContext
    :: IO ContextRef
foreign import ccall unsafe "LLVMContextDispose" contextDispose
    :: ContextRef -> IO ()
foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext
    :: ContextRef -> CString -> CUInt -> IO CUInt
foreign import ccall unsafe "LLVMGetMDKindID" getMDKindID
    :: CString -> CUInt -> IO CUInt

-- ** Attributes

foreign import ccall unsafe "LLVMGetEnumAttributeKindForName" getEnumAttributeKindForName
    :: CString -> C.CSize -> IO AttributeKind

foreign import ccall unsafe "LLVMGetLastEnumAttributeKind" getLastEnumAttributeKind
    :: IO AttributeKind

foreign import ccall unsafe "LLVMCreateEnumAttribute" createEnumAttribute
    :: ContextRef -> AttributeKind -> Word64 -> IO AttributeRef

foreign import ccall unsafe "LLVMGetEnumAttributeKind" getEnumAttributeKind
    :: AttributeRef -> IO AttributeKind

foreign import ccall unsafe "LLVMGetEnumAttributeValue" getEnumAttributeValue
    :: AttributeRef -> IO Word64

foreign import ccall unsafe "LLVMCreateStringAttribute" createStringAttribute
    :: ContextRef -> CString -> CUInt -> CString -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMGetStringAttributeKind" getStringAttributeKind
    :: AttributeRef -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVMGetStringAttributeValue" getStringAttributeValue
    :: AttributeRef -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVMIsEnumAttribute" isEnumAttribute
    :: AttributeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsStringAttribute" isStringAttribute
    :: AttributeRef -> IO LLVM.Bool

-- ** Modules
foreign import ccall unsafe "LLVMModuleCreateWithName" moduleCreateWithName
    :: CString -> IO ModuleRef
foreign import ccall unsafe "LLVMModuleCreateWithNameInContext"
        moduleCreateWithNameInContext
    :: CString -> ContextRef -> IO ModuleRef
foreign import ccall unsafe "LLVMDisposeModule" disposeModule
    :: ModuleRef -> IO ()
foreign import ccall unsafe "&LLVMDisposeModule" ptrDisposeModule
    :: FunPtr (ModuleRef -> IO ())

-- ** Data Layout
foreign import ccall unsafe "LLVMGetDataLayout" getDataLayout
    :: ModuleRef -> IO CString
foreign import ccall unsafe "LLVMSetDataLayout" setDataLayout
    :: ModuleRef -> CString -> IO ()

-- ** Targets
foreign import ccall unsafe "LLVMGetTarget" getTarget
    :: ModuleRef -> IO CString
foreign import ccall unsafe "LLVMSetTarget" setTarget
    :: ModuleRef -> CString -> IO ()

-- ** Dump module
foreign import ccall unsafe "LLVMDumpModule" dumpModule
    :: ModuleRef -> IO ()
foreign import ccall unsafe "LLVMSetModuleInlineAsm" setModuleInlineAsm
    :: ModuleRef -> CString -> IO ()
foreign import ccall unsafe "LLVMGetModuleContext" getModuleContext
    :: ModuleRef -> IO ContextRef

-- ** Functions
foreign import ccall unsafe "LLVMAddFunction" addFunction
    :: ModuleRef                -- ^ module
    -> CString                  -- ^ name
    -> TypeRef                  -- ^ type
    -> IO ValueRef
foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction
    :: ModuleRef                -- ^ module
    -> CString                  -- ^ name
    -> IO ValueRef              -- ^ function (@nullPtr@ if not found)
foreign import ccall unsafe "LLVMGetFirstFunction" getFirstFunction
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetLastFunction" getLastFunction
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextFunction" getNextFunction
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousFunction" getPreviousFunction
    :: ValueRef -> IO ValueRef


-- ** Types
foreign import ccall unsafe "LLVMGetTypeKind" getTypeKindCUInt
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMTypeIsSized" typeIsSized
    :: TypeRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMGetTypeContext" getTypeContext
    :: TypeRef -> IO ContextRef

-- ** Integer types
foreign import ccall unsafe "LLVMInt1TypeInContext" int1TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt8TypeInContext" int8TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt16TypeInContext" int16TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt32TypeInContext" int32TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt64TypeInContext" int64TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMIntTypeInContext" intTypeInContext
    :: ContextRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMInt1Type" int1Type :: IO TypeRef
foreign import ccall unsafe "LLVMInt8Type" int8Type :: IO TypeRef
foreign import ccall unsafe "LLVMInt16Type" int16Type :: IO TypeRef
foreign import ccall unsafe "LLVMInt32Type" int32Type :: IO TypeRef
foreign import ccall unsafe "LLVMInt64Type" int64Type :: IO TypeRef
foreign import ccall unsafe "LLVMIntType" integerType :: CUInt -> IO TypeRef
foreign import ccall unsafe "LLVMGetIntTypeWidth" getIntTypeWidth
    :: TypeRef -> IO CUInt

-- ** Real types
foreign import ccall unsafe "LLVMFloatTypeInContext" floatTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMDoubleTypeInContext" doubleTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMX86FP80TypeInContext" x86FP80TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMFP128TypeInContext" fp128TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMPPCFP128TypeInContext" ppcFP128TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMFloatType" floatType :: IO TypeRef
foreign import ccall unsafe "LLVMDoubleType" doubleType :: IO TypeRef
foreign import ccall unsafe "LLVMX86FP80Type" x86FP80Type :: IO TypeRef
foreign import ccall unsafe "LLVMFP128Type" fp128Type :: IO TypeRef
foreign import ccall unsafe "LLVMPPCFP128Type" ppcFP128Type :: IO TypeRef

-- ** Function types
-- | Create a function type.
foreign import ccall unsafe "LLVMFunctionType" functionType
        :: TypeRef              -- ^ return type
        -> Ptr TypeRef          -- ^ array of argument types
        -> CUInt                -- ^ number of elements in array
        -> LLVM.Bool                 -- ^ non-zero if function is varargs
        -> IO TypeRef

-- | Indicate whether a function takes varargs.
foreign import ccall unsafe "LLVMIsFunctionVarArg" isFunctionVarArg
        :: TypeRef -> IO LLVM.Bool

-- | Give a function's return type.
foreign import ccall unsafe "LLVMGetReturnType" getReturnType
        :: TypeRef -> IO TypeRef

-- | Give the number of fixed parameters that a function takes.
foreign import ccall unsafe "LLVMCountParamTypes" countParamTypes
        :: TypeRef -> IO CUInt

-- | Fill out an array with the types of a function's fixed
-- parameters.
foreign import ccall unsafe "LLVMGetParamTypes" getParamTypes
        :: TypeRef -> Ptr TypeRef -> IO ()

-- ** Struct Type
foreign import ccall unsafe "LLVMStructTypeInContext" structTypeInContext
    :: ContextRef -> (Ptr TypeRef) -> CUInt -> LLVM.Bool -> IO TypeRef
foreign import ccall unsafe "LLVMStructType" structType
    :: Ptr TypeRef -> CUInt -> LLVM.Bool -> IO TypeRef
foreign import ccall unsafe "LLVMStructCreateNamed" structCreateNamed
    :: ContextRef -> CString -> IO TypeRef
foreign import ccall unsafe "LLVMGetStructName" getStructName
    :: TypeRef -> IO CString
foreign import ccall unsafe "LLVMStructSetBody" structSetBody
    :: TypeRef -> Ptr TypeRef -> CUInt -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMCountStructElementTypes"
    countStructElementTypes :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetStructElementTypes" getStructElementTypes
    :: TypeRef -> Ptr TypeRef -> IO ()
foreign import ccall unsafe "LLVMIsPackedStruct" isPackedStruct
    :: TypeRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMIsOpaqueStruct" isOpaqueStruct
    :: TypeRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMGetTypeByName" getTypeByName
    :: ModuleRef -> CString -> IO TypeRef

-- ** Array, Pointer, and Vector types
foreign import ccall unsafe "LLVMArrayType" arrayType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> IO TypeRef
foreign import ccall unsafe "LLVMPointerType" pointerType
    :: TypeRef                  -- ^ pointed-to type
    -> CUInt                    -- ^ address space
    -> IO TypeRef
foreign import ccall unsafe "LLVMVectorType" vectorType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> IO TypeRef


-- | Get the type of a sequential type's elements.
foreign import ccall unsafe "LLVMGetElementType" getElementType
    :: TypeRef -> IO TypeRef
foreign import ccall unsafe "LLVMGetArrayLength" getArrayLength
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetPointerAddressSpace" getPointerAddressSpace
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetVectorSize" getVectorSize
    :: TypeRef -> IO CUInt


-- ** Other Types

foreign import ccall unsafe "LLVMVoidTypeInContext" voidTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMLabelTypeInContext" labelTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMX86MMXTypeInContext" x86MMXTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMVoidType" voidType :: IO TypeRef
foreign import ccall unsafe "LLVMLabelType" labelType :: IO TypeRef
foreign import ccall unsafe "LLVMX86MMXType" x86MMXType :: IO TypeRef

-- ** Values
foreign import ccall unsafe "LLVMTypeOf" typeOf
    :: ValueRef -> IO TypeRef
foreign import ccall unsafe "LLVMGetValueName" getValueName
    :: ValueRef -> IO CString
foreign import ccall unsafe "LLVMSetValueName" setValueName
    :: ValueRef -> CString -> IO ()
foreign import ccall unsafe "LLVMDumpValue" dumpValue
    :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMReplaceAllUsesWith" replaceAllUsesWith
    :: ValueRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMHasMetadata" hasMetadata
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMGetMetadata" getMetadata
    :: ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMSetMetadata" setMetadata
    :: ValueRef -> CUInt -> ValueRef -> IO ()

-- ** Uses
foreign import ccall unsafe "LLVMGetFirstUse" getFirstUse
    :: ValueRef -> IO UseRef
foreign import ccall unsafe "LLVMGetNextUse" getNextUse
    :: UseRef -> IO UseRef
foreign import ccall unsafe "LLVMGetUser" getUser
    :: UseRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetUsedValue" getUsedValue
    :: UseRef -> IO ValueRef

-- ** Users
foreign import ccall unsafe "LLVMGetOperand" getOperand
    :: ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMSetOperand" setOperand
    :: ValueRef -> CUInt -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMGetNumOperands" getNumOperands
    :: ValueRef -> IO CUInt

-- ** Constants
foreign import ccall unsafe "LLVMConstNull" constNull
    :: TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstAllOnes" constAllOnes
    :: TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetUndef" getUndef
    :: TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMIsConstant" isConstant
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMIsUndef" isUndef
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMIsNull" isNull
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMConstPointerNull" constPointerNull
    :: TypeRef -> IO ValueRef

-- ** Metadata
foreign import ccall unsafe "LLVMMDStringInContext" mDStringInContext
    :: ContextRef -> CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMMDString" mDString
    :: CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMMDNodeInContext" mDNodeInContext
    :: ContextRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMMDNode" mDNode
    :: (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMGetMDString" getMDString
    :: ValueRef -> Ptr CUInt -> IO CString
{-
foreign import ccall unsafe "LLVMGetMDNodeNumOperands" getMDNodeNumOperands
    :: ValueRef -> IO (CInt)
foreign import ccall unsafe "LLVMGetMDNodeOperand" getMDNodeOperand
    :: ValueRef -> CUInt -> IO (Ptr ValueRef)
-}
foreign import ccall unsafe "LLVMGetNamedMetadataNumOperands" getNamedMetadataNumOperands
    :: ModuleRef -> CString -> IO CUInt
foreign import ccall unsafe "LLVMGetNamedMetadataOperands" getNamedMetadataOperands
    :: ModuleRef -> CString -> Ptr ValueRef -> IO ()

-- ** Scalar Constants
foreign import ccall unsafe "LLVMConstInt" constInt
    :: TypeRef -> CULLong -> LLVM.Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntOfArbitraryPrecision" constIntOfArbitraryPrecision
    :: TypeRef -> CUInt -> Ptr CULLong -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntOfString" constIntOfString
    :: TypeRef -> CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntOfStringAndSize" constIntOfStringAndSize
    :: TypeRef -> CString -> CUInt -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstReal" constReal
    :: TypeRef -> CDouble -> IO ValueRef
foreign import ccall unsafe "LLVMConstRealOfString" constRealOfString
    :: TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMConstRealOfStringAndSize" constRealOfStringAndSize
    :: TypeRef -> CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntGetZExtValue" constIntGetZExtValue
    :: ValueRef -> IO CULLong
foreign import ccall unsafe "LLVMConstIntGetSExtValue" constIntGetSExtValue
    :: ValueRef -> IO CLLong

-- ** Composite Constants
foreign import ccall unsafe "LLVMConstStringInContext" constStringInContext
    :: ContextRef -> CString -> CUInt -> LLVM.Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstStructInContext" constStructInContext
    :: ContextRef -> (Ptr ValueRef) -> CUInt -> LLVM.Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstString" constString
    :: CString -> CUInt -> LLVM.Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstArray" constArray
    :: TypeRef -> Ptr ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstStruct" constStruct
    :: Ptr ValueRef -> CUInt -> LLVM.Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstNamedStruct" constNamedStruct
    :: TypeRef -> Ptr ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstVector" constVector
    :: Ptr ValueRef -> CUInt -> IO ValueRef

-- ** Constant expressions
foreign import ccall unsafe "LLVMGetConstOpcode" getConstOpcode
    :: ValueRef -> IO CUInt {-Opcode-}
foreign import ccall unsafe "LLVMAlignOf" alignOf
    :: TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMSizeOf" sizeOf
    :: TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNeg" constNeg
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNSWNeg" constNSWNeg
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWNeg" constNUWNeg
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFNeg" constFNeg
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNot" constNot
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstAdd" constAdd
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNSWAdd" constNSWAdd
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWAdd" constNUWAdd
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFAdd" constFAdd
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSub" constSub
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNSWSub" constNSWSub
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWSub" constNUWSub
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFSub" constFSub
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstMul" constMul
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNSWMul" constNSWMul
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWMul" constNUWMul
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFMul" constFMul
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstUDiv" constUDiv
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSDiv" constSDiv
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstExactSDiv" constExactSDiv
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFDiv" constFDiv
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstURem" constURem
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSRem" constSRem
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFRem" constFRem
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstAnd" constAnd
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstOr" constOr
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstXor" constXor
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstICmp" constICmp
    :: CInt -> ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFCmp" constFCmp
    :: CInt -> ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstShl" constShl
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstLShr" constLShr
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstAShr" constAShr
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstGEP" constGEP
    :: ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstInBoundsGEP" constInBoundsGEP
    :: ValueRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstTrunc" constTrunc
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSExt" constSExt
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstZExt" constZExt
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFPTrunc" constFPTrunc
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFPExt" constFPExt
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstUIToFP" constUIToFP
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSIToFP" constSIToFP
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFPToUI" constFPToUI
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFPToSI" constFPToSI
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstPtrToInt" constPtrToInt
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntToPtr" constIntToPtr
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstBitCast" constBitCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSExtOrBitCast" constSExtOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstZExtOrBitCast" constZExtOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstTruncOrBitCast" constTruncOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstPointerCast" constPointerCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntCast" constIntCast
    :: ValueRef -> TypeRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstFPCast" constFPCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSelect" constSelect
    :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstExtractElement" constExtractElement
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstInsertElement" constInsertElement
    :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstShuffleVector" constShuffleVector
    :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstExtractValue" constExtractValue
    :: ValueRef -> Ptr CUInt -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstInsertValue" constInsertValue
    :: ValueRef -> ValueRef -> Ptr CUInt -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstInlineAsm" constInlineAsm
    :: TypeRef -> CString -> CString -> LLVM.Bool -> LLVM.Bool -> IO ValueRef
foreign import ccall unsafe "LLVMBlockAddress" blockAddress
    :: ValueRef -> BasicBlockRef -> IO ValueRef

-- ** Operations on globals
foreign import ccall unsafe "LLVMGetGlobalParent" getGlobalParent
    :: ValueRef -> IO ModuleRef
foreign import ccall unsafe "LLVMIsDeclaration" isDeclaration
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMGetLinkage" getLinkage
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMSetLinkage" setLinkage
    :: ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMGetSection" getSection
    :: ValueRef -> IO CString
foreign import ccall unsafe "LLVMSetSection" setSection
    :: ValueRef -> CString -> IO ()
foreign import ccall unsafe "LLVMGetVisibility" getVisibility
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMSetVisibility" setVisibility
    :: ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMGetAlignment" getAlignment
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMSetAlignment" setAlignment
    :: ValueRef -> CUInt -> IO ()

-- ** Global Variables
foreign import ccall unsafe "LLVMAddGlobal" addGlobal
    :: ModuleRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMAddGlobalInAddressSpace" addGlobalInAddressSpace
    :: ModuleRef -> TypeRef -> CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMGetNamedGlobal" getNamedGlobal
    :: ModuleRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMGetFirstGlobal" getFirstGlobal
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetLastGlobal" getLastGlobal
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextGlobal" getNextGlobal
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousGlobal" getPreviousGlobal
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMDeleteGlobal" deleteGlobal
    :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMSetInitializer" setInitializer
    :: ValueRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMGetInitializer" getInitializer
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMIsThreadLocal" isThreadLocal
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMSetThreadLocal" setThreadLocal
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMIsGlobalConstant" isGlobalConstant
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMSetGlobalConstant" setGlobalConstant
    :: ValueRef -> LLVM.Bool -> IO ()

-- ** Aliases
foreign import ccall unsafe "LLVMAddAlias" addAlias
    :: ModuleRef -> TypeRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteFunction" deleteFunction
    :: ValueRef                 -- ^ function
    -> IO ()
foreign import ccall unsafe "LLVMGetIntrinsicID" getIntrinsicID
    :: ValueRef                 -- ^ function
    -> IO CUInt
foreign import ccall unsafe "LLVMGetFunctionCallConv" getFunctionCallConv
    :: ValueRef                 -- ^ function
    -> IO CUInt
foreign import ccall unsafe "LLVMSetFunctionCallConv" setFunctionCallConv
    :: ValueRef                 -- ^ function
    -> CUInt
    -> IO ()
foreign import ccall unsafe "LLVMGetGC" getGC
    :: ValueRef -> IO CString
foreign import ccall unsafe "LLVMSetGC" setGC
    :: ValueRef -> CString -> IO ()

-- ** Attribute attachment

foreign import ccall unsafe "LLVMAddAttributeAtIndex" addAttributeAtIndex
    :: ValueRef -> AttributeIndex -> AttributeRef -> IO ()

foreign import ccall unsafe "LLVMGetAttributeCountAtIndex" getAttributeCountAtIndex
    :: ValueRef -> AttributeIndex -> IO CUInt

foreign import ccall unsafe "LLVMGetAttributesAtIndex" getAttributesAtIndex
    :: ValueRef -> AttributeIndex -> (Ptr AttributeRef) -> IO ()

foreign import ccall unsafe "LLVMGetEnumAttributeAtIndex" getEnumAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMGetStringAttributeAtIndex" getStringAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMRemoveEnumAttributeAtIndex" removeEnumAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CUInt -> IO ()

foreign import ccall unsafe "LLVMRemoveStringAttributeAtIndex" removeStringAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO ()

foreign import ccall unsafe "LLVMAddTargetDependentFunctionAttr" addTargetDependentFunctionAttr
    :: ValueRef -> CString -> CString -> IO ()

-- ** Parameters
foreign import ccall unsafe "LLVMCountParams" countParams
    :: ValueRef                 -- ^ function
    -> IO CUInt
foreign import ccall unsafe "LLVMGetParams" getParams
    :: ValueRef                 -- ^ function
    -> Ptr ValueRef             -- ^ array to fill out
    -> IO ()
foreign import ccall unsafe "LLVMGetParam" getParam
    :: ValueRef                 -- ^ function
    -> CUInt                    -- ^ offset into array
    -> IO ValueRef
foreign import ccall unsafe "LLVMGetParamParent" getParamParent
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetFirstParam" getFirstParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetLastParam" getLastParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextParam" getNextParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousParam" getPreviousParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMSetParamAlignment" setParamAlignment
    :: ValueRef -> CUInt -> IO ()

-- ** Basic Blocks
foreign import ccall unsafe "LLVMBasicBlockAsValue" basicBlockAsValue
    :: BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMValueIsBasicBlock" valueIsBasicBlock
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMValueAsBasicBlock" valueAsBasicBlock
    :: ValueRef                 -- ^ basic block
    -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetBasicBlockParent" getBasicBlockParent
    :: BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetBasicBlockTerminator" getBasicBlockTerminator
    :: BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMCountBasicBlocks" countBasicBlocks
    :: ValueRef                 -- ^ function
    -> IO CUInt
foreign import ccall unsafe "LLVMGetBasicBlocks" getBasicBlocks
    :: ValueRef                 -- ^ function
    -> Ptr BasicBlockRef        -- ^ array to fill out
    -> IO ()
foreign import ccall unsafe "LLVMGetFirstBasicBlock" getFirstBasicBlock
    :: ValueRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetLastBasicBlock" getLastBasicBlock
    :: ValueRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetNextBasicBlock" getNextBasicBlock
    :: BasicBlockRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetPreviousBasicBlock" getPreviousBasicBlock
    :: BasicBlockRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetEntryBasicBlock" getEntryBasicBlock
    :: ValueRef                 -- ^ function
    -> IO BasicBlockRef
foreign import ccall unsafe "LLVMAppendBasicBlockInContext" appendBasicBlockInContext
    :: ContextRef -> ValueRef -> CString -> IO BasicBlockRef
foreign import ccall unsafe "LLVMInsertBasicBlockInContext" insertBasicBlockInContext
    :: ContextRef -> BasicBlockRef -> CString -> IO BasicBlockRef
foreign import ccall unsafe "LLVMAppendBasicBlock" appendBasicBlock
    :: ValueRef                 -- ^ function
    -> CString                  -- ^ name for label
    -> IO BasicBlockRef
foreign import ccall unsafe "LLVMInsertBasicBlock" insertBasicBlock
    :: BasicBlockRef            -- ^ insert before this one
    -> CString                  -- ^ name for label
    -> IO BasicBlockRef
foreign import ccall unsafe "LLVMDeleteBasicBlock" deleteBasicBlock
    :: BasicBlockRef -> IO ()
foreign import ccall unsafe "LLVMRemoveBasicBlockFromParent" removeBasicBlockFromParent
    :: BasicBlockRef -> IO ()
foreign import ccall unsafe "LLVMMoveBasicBlockBefore" moveBasicBlockBefore
    :: BasicBlockRef -> BasicBlockRef -> IO ()
foreign import ccall unsafe "LLVMMoveBasicBlockAfter" moveBasicBlockAfter
    :: BasicBlockRef -> BasicBlockRef -> IO ()
foreign import ccall unsafe "LLVMGetFirstInstruction" getFirstInstruction
    :: BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetLastInstruction" getLastInstruction
    :: BasicBlockRef -> IO ValueRef

-- ** Instructions
foreign import ccall unsafe "LLVMGetInstructionParent" getInstructionParent
    :: ValueRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetNextInstruction" getNextInstruction
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousInstruction" getPreviousInstruction
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMInstructionEraseFromParent" instructionEraseFromParent
    :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMGetInstructionOpcode" getInstructionOpcode
    :: ValueRef -> IO Int
foreign import ccall unsafe "LLVMGetICmpPredicate" getICmpPredicate
    :: ValueRef -> IO Int

-- ** Call sites
foreign import ccall unsafe "LLVMSetInstructionCallConv" setInstructionCallConv
    :: ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMGetInstructionCallConv" getInstructionCallConv
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMSetInstrParamAlignment" setInstrParamAlignment
    :: ValueRef -> CUInt -> CUInt -> IO ()

foreign import ccall unsafe "LLVMAddCallSiteAttribute" addCallSiteAttribute
    :: ValueRef -> AttributeIndex -> AttributeRef -> IO ()

foreign import ccall unsafe "LLVMGetCallSiteAttributeCount" getCallSiteAttributeCount
    :: ValueRef -> AttributeIndex -> IO CUInt

foreign import ccall unsafe "LLVMGetCallSiteAttributes" getCallSiteAttributes
    :: ValueRef -> AttributeIndex -> (Ptr AttributeRef) -> IO ()

foreign import ccall unsafe "LLVMGetCallSiteEnumAttribute" getCallSiteEnumAttribute
    :: ValueRef -> AttributeIndex -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMGetCallSiteStringAttribute" getCallSiteStringAttribute
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMRemoveCallSiteEnumAttribute" removeCallSiteEnumAttribute
    :: ValueRef -> AttributeIndex -> CUInt -> IO ()

foreign import ccall unsafe "LLVMRemoveCallSiteStringAttribute" removeCallSiteStringAttribute
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetCalledValue" getCalledValue
    :: ValueRef -> IO ValueRef

-- ** Call instructions
foreign import ccall unsafe "LLVMIsTailCall" isTailCall
    :: ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMSetTailCall" setTailCall
    :: ValueRef -> LLVM.Bool -> IO ()

-- ** Switch Instructions
foreign import ccall unsafe "LLVMGetSwitchDefaultDest" getSwitchDefaultDest
    :: ValueRef -> IO BasicBlockRef

-- ** Phi Nodes
foreign import ccall unsafe "LLVMAddIncoming" addIncoming
    :: ValueRef -> Ptr ValueRef -> Ptr ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMCountIncoming" countIncoming
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMGetIncomingValue" getIncomingValue
    :: ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMGetIncomingBlock" getIncomingBlock
    :: ValueRef -> CUInt -> IO BasicBlockRef

-- ** Builders
foreign import ccall unsafe "LLVMCreateBuilderInContext" createBuilderInContext
    :: ContextRef -> IO BuilderRef
foreign import ccall unsafe "LLVMCreateBuilder" createBuilder
    :: IO BuilderRef
foreign import ccall unsafe "LLVMPositionBuilder" positionBuilder
    :: BuilderRef -> BasicBlockRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMPositionBuilderBefore" positionBefore
    :: BuilderRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMPositionBuilderAtEnd" positionAtEnd
    :: BuilderRef -> BasicBlockRef -> IO ()
foreign import ccall unsafe "LLVMGetInsertBlock" getInsertBlock
    :: BuilderRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMClearInsertionPosition" clearInsertionPosition
    :: BuilderRef -> IO ()
foreign import ccall unsafe "LLVMInsertIntoBuilder" insertIntoBuilder
    :: BuilderRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMInsertIntoBuilderWithName" insertIntoBuilderWithName
    :: BuilderRef -> ValueRef -> CString -> IO ()
foreign import ccall unsafe "&LLVMDisposeBuilder" ptrDisposeBuilder
    :: FunPtr (BuilderRef -> IO ())

-- ** Metadata
foreign import ccall unsafe "LLVMGetCurrentDebugLocation" getCurrentDebugLocation
    :: BuilderRef -> IO ValueRef
foreign import ccall unsafe "LLVMSetCurrentDebugLocation" setCurrentDebugLocation
    :: BuilderRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMSetInstDebugLocation" setInstDebugLocation
    :: BuilderRef -> ValueRef -> IO ()

-- ** Terminators
foreign import ccall unsafe "LLVMBuildRetVoid" buildRetVoid
    :: BuilderRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildRet" buildRet
    :: BuilderRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAggregateRet" buildAggregateRet
    :: BuilderRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMBuildBr" buildBr
    :: BuilderRef -> BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildCondBr" buildCondBr
    :: BuilderRef -> ValueRef -> BasicBlockRef -> BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSwitch" buildSwitch
    :: BuilderRef -> ValueRef -> BasicBlockRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIndirectBr" buildIndirectBr
    :: BuilderRef -> ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInvoke" buildInvoke
    :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt
    -> BasicBlockRef -> BasicBlockRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildLandingPad" buildLandingPad
    :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildResume" buildResume
    :: BuilderRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUnreachable" buildUnreachable
    :: BuilderRef -> IO ValueRef

-- ** Switch instructions
foreign import ccall unsafe "LLVMAddCase" addCase
    :: ValueRef -> ValueRef -> BasicBlockRef -> IO ()

-- ** IndirectBr instructions
foreign import ccall unsafe "LLVMAddDestination" addDestination
    :: ValueRef -> BasicBlockRef -> IO ()

-- ** LandingPad instructions
foreign import ccall unsafe "LLVMAddClause" addClause
    :: ValueRef -> ValueRef -> IO ()

-- ** Resume instructions
foreign import ccall unsafe "LLVMSetCleanup" setCleanup
    :: ValueRef -> LLVM.Bool -> IO ()

-- ** Arithmetic
foreign import ccall unsafe "LLVMBuildAdd" buildAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWAdd" buildNSWAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWAdd" buildNUWAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFAdd" buildFAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSub" buildSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWSub" buildNSWSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWSub" buildNUWSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFSub" buildFSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildMul" buildMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWMul" buildNSWMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWMul" buildNUWMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFMul" buildFMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUDiv" buildUDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSDiv" buildSDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildExactSDiv" buildExactSDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFDiv" buildFDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildURem" buildURem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSRem" buildSRem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFRem" buildFRem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildShl" buildShl
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildLShr" buildLShr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAShr" buildAShr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAnd" buildAnd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildOr" buildOr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildXor" buildXor
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildBinOp" buildBinOp
    :: BuilderRef -> CUInt{-Opcode-} -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNeg" buildNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWNeg" buildNSWNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWNeg" buildNUWNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFNeg" buildFNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNot" buildNot
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

-- ** Floating point attributes
foreign import ccall unsafe "LLVMSetHasUnsafeAlgebra" setFastMath
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasUnsafeAlgebra" setHasUnsafeAlgebra
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasNoNaNs" setHasNoNaNs
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasNoInfs" setHasNoInfs
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasNoSignedZeros" setHasNoSignedZeros
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasAllowReciprocal" setHasAllowReciprocal
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasAllowReassoc" setHasAllowReassoc
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasApproxFunc" setHasApproxFunc
    :: ValueRef -> LLVM.Bool -> IO ()


-- ** Memory
foreign import ccall unsafe "LLVMBuildMalloc" buildMalloc
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildArrayMalloc" buildArrayMalloc
    :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAlloca" buildAlloca
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildArrayAlloca" buildArrayAlloca
    :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFree" buildFree
    :: BuilderRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildLoad" buildLoad
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildStore" buildStore
    :: BuilderRef -> ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildGEP" buildGEP
    :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString
    -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInBoundsGEP" buildInBoundsGEP
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildStructGEP" buildStructGEP
    :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildGlobalString" buildGlobalString
    :: BuilderRef -> CString -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildGlobalStringPtr" buildGlobalStringPtr
    :: BuilderRef -> CString -> CString -> IO ValueRef

-- Casts
foreign import ccall unsafe "LLVMBuildTrunc" buildTrunc
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildZExt" buildZExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSExt" buildSExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPToUI" buildFPToUI
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPToSI" buildFPToSI
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUIToFP" buildUIToFP
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSIToFP" buildSIToFP
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPTrunc" buildFPTrunc
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPExt" buildFPExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildPtrToInt" buildPtrToInt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIntToPtr" buildIntToPtr
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildBitCast" buildBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildZExtOrBitCast" buildZExtOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSExtOrBitCast" buildSExtOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildTruncOrBitCast" buildTruncOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildCast" buildCast
    :: BuilderRef -> CUInt{-Opcode-} -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildPointerCast" buildPointerCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIntCast" buildIntCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPCast" buildFPCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

-- Comparisons
foreign import ccall unsafe "LLVMBuildICmp" buildICmp
    :: BuilderRef -> CInt -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFCmp" buildFCmp
    :: BuilderRef -> CInt -> ValueRef -> ValueRef -> CString -> IO ValueRef

-- Miscellaneous instructions
foreign import ccall unsafe "LLVMBuildPhi" buildPhi
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildCall" buildCall
    :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSelect" buildSelect
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildVAArg" buildVAArg
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildExtractElement" buildExtractElement
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInsertElement" buildInsertElement
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildShuffleVector" buildShuffleVector
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildExtractValue" buildExtractValue
    :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInsertValue" buildInsertValue
    :: BuilderRef -> ValueRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIsNull" buildIsNull
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIsNotNull" buildIsNotNull
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildPtrDiff" buildPtrDiff
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef


-- ** Memory Buffers
foreign import ccall unsafe "LLVMCreateMemoryBufferWithContentsOfFile" createMemoryBufferWithContentsOfFile
    :: CString -> Ptr MemoryBufferRef -> Ptr CString -> IO LLVM.Bool
foreign import ccall unsafe "LLVMCreateMemoryBufferWithSTDIN" createMemoryBufferWithSTDIN
    :: Ptr MemoryBufferRef -> Ptr CString -> IO LLVM.Bool
foreign import ccall unsafe "LLVMDisposeMemoryBuffer" disposeMemoryBuffer
    :: MemoryBufferRef -> IO ()

-- ** Pass Registry
foreign import ccall unsafe "LLVMGetGlobalPassRegistry" getGlobalPassRegistry
    :: IO PassRegistryRef

-- ** Pass Managers
foreign import ccall unsafe "LLVMCreatePassManager" createPassManager
    :: IO PassManagerRef
foreign import ccall unsafe "LLVMCreateFunctionPassManagerForModule" createFunctionPassManagerForModule
    :: ModuleRef -> IO PassManagerRef
foreign import ccall unsafe "LLVMRunPassManager" runPassManager
    :: PassManagerRef -> ModuleRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMInitializeFunctionPassManager" initializeFunctionPassManager
    :: PassManagerRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMRunFunctionPassManager" runFunctionPassManager
    :: PassManagerRef -> ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMFinalizeFunctionPassManager" finalizeFunctionPassManager
    :: PassManagerRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMDisposePassManager" disposePassManager
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "&LLVMDisposePassManager" ptrDisposePassManager
    :: FunPtr (PassManagerRef -> IO ())

-- ** Functions from extras.cpp
foreign import ccall unsafe "LLVMValueGetNumUses" getNumUses
    :: ValueRef -> IO CInt
foreign import ccall unsafe "LLVMInstGetOpcode" instGetOpcode
    :: ValueRef -> IO CInt
foreign import ccall unsafe "LLVMCmpInstGetPredicate" cmpInstGetPredicate
    :: ValueRef -> IO CInt
