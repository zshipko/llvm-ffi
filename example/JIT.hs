{-# LANGUAGE ForeignFunctionInterface #-}
{- |
This program has two purposes:
First a minimalistic demonstration of how to generate and run code with LLVM.
Second a test program that forces to run the linker.
It let us check whether Haskell bindings match C functions.
-}
module Main where

import qualified LLVM.FFI.Transforms.PassManagerBuilder as PMB
import qualified LLVM.FFI.Transforms.Scalar as Transform
import qualified LLVM.FFI.ExecutionEngine as EE
import qualified LLVM.FFI.BitWriter as BW
import qualified LLVM.FFI.Core as Core
import qualified LLVM.Target.Native as Native

import qualified Foreign.C.String as CStr
import qualified Foreign.Marshal.Array as Array
import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.C.String (withCString)
import Foreign.C.Types (CUInt, CFloat)
import Foreign.Storable (Storable, peek, sizeOf)
import Foreign.Ptr (Ptr, FunPtr)

import Control.Exception (bracket, bracket_, finally)
import Control.Monad (when, void)

import qualified System.Exit as Exit
import Text.Printf (printf)


vectorSize :: Int
roundName :: String

(vectorSize, roundName) =
   if False
     then (4, "llvm.x86.sse41.round.ps")
     else (8, "llvm.x86.avx.round.ps.256")

withArrayLen :: (Storable a) => [a] -> (CUInt -> Ptr a -> IO b) -> IO b
withArrayLen xs act =
   Array.withArrayLen xs $ \len ptr -> act (fromIntegral len) ptr

noResult :: IO () -> IO ()
noResult = id

type Importer f = FunPtr f -> f

foreign import ccall safe "dynamic" derefFuncPtr :: Importer (Ptr a -> IO ())



main :: IO ()
main = do
   Native.initializeNativeTarget

   modul <- withCString "_module" Core.moduleCreateWithName
   withCString Core.hostTriple $ Core.setTarget modul
   vectorType <-
      if True
        then Core.floatType >>= flip Core.vectorType (fromIntegral vectorSize)
        else Core.floatType

   ptrType <- Core.pointerType vectorType 0
   voidType <- Core.voidType
   let params = [ptrType]
   roundType <-
      withArrayLen params $ \len ps ->
         Core.functionType voidType ps len Core.false
   func <- withCString "round" $ \name -> Core.addFunction modul name roundType
   Core.setLinkage func $ Core.fromLinkage Core.ExternalLinkage
   builder <- Core.createBuilder
   block <- withCString "_L1" $ Core.appendBasicBlock func
   Core.positionAtEnd builder block
   param <- Core.getParam func 0
   loaded <- withCString "" $ Core.buildLoad builder param
   int32Type <- Core.int32Type
   let funcParams = [vectorType, int32Type]
   funcType <-
      withArrayLen funcParams $ \len ps ->
         Core.functionType vectorType ps len Core.false
   roundFunc <-
      withCString roundName $ \name -> Core.addFunction modul name funcType
   Core.setLinkage roundFunc $ Core.fromLinkage Core.ExternalLinkage
   callRound <-
      if True
        then do
            const1 <- Core.constInt int32Type 1 Core.false
            let callParams = [loaded, const1]
            call <-
               withArrayLen callParams $ \len ps ->
               withCString "" $ Core.buildCall builder roundFunc ps len
            Core.setInstructionCallConv call $
               Core.fromCallingConvention Core.C
            Core.addInstrAttribute call 0 0
            return call
        else do
           void $ withCString "" $ Core.buildFAdd builder loaded loaded
           zero <- Core.constNull vectorType
           add <- withCString "" $ Core.buildFAdd builder loaded zero
           Core.setHasNoSignedZeros add Core.true
           return add

   void $ Core.buildStore builder callRound param
   void $ Core.buildRetVoid builder

   void $ withCString "round-avx.bc" $ BW.writeBitcodeToFile modul

   when True $
      bracket Core.createPassManager Core.disposePassManager $ \mpasses ->
      bracket (Core.createFunctionPassManagerForModule modul)
            Core.disposePassManager $ \fpasses -> do
         Transform.addVerifierPass mpasses

         bracket PMB.create PMB.dispose $ \passBuilder -> do
            PMB.setOptLevel passBuilder 3
            PMB.populateFunctionPassManager passBuilder fpasses
            PMB.populateModulePassManager passBuilder mpasses

         bracket_
            (Core.initializeFunctionPassManager fpasses)
            (Core.finalizeFunctionPassManager fpasses)
            (void $ Core.runFunctionPassManager fpasses func)
         void $ Core.runPassManager mpasses modul

         void $ withCString "round-avx-opt.bc" $ BW.writeBitcodeToFile modul

   Alloc.alloca $ \execEngineRef -> do
      Alloc.alloca $ \errorMsgRef -> do
         err <-
            EE.createExecutionEngineForModuleCPU execEngineRef modul errorMsgRef
         when (err/=Core.false) $ do
            noResult $
               printf "Core.createExecutionEngine: %s\n"
                  =<< CStr.peekCString =<< peek errorMsgRef
            Exit.exitFailure

      execEngine <- peek execEngineRef
      flip finally (EE.disposeExecutionEngine execEngine) $ do
         let vector = take vectorSize $ iterate (1+) (-1.3 :: CFloat)
         funcPtr <- EE.getPointerToFunction execEngine func
         let size = sum $ map sizeOf vector
         Alloc.allocaBytesAligned size size $ \ptr -> do
            Array.pokeArray ptr vector
            derefFuncPtr funcPtr ptr
            print =<< Array.peekArray vectorSize ptr
