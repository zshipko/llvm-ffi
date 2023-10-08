{-# LANGUAGE ForeignFunctionInterface #-}
{- |
This program has two purposes:
First a minimalistic demonstration of
how to generate and run and optimize code with LLVM.
Second a test program that forces to run the linker.
It let us check whether Haskell bindings match C functions.
-}
module Main where

import Common (withArrayLen, createExecutionEngine)

import qualified LLVM.FFI.Transforms.PassBuilder as PB
import qualified LLVM.FFI.TargetMachine as TM
import qualified LLVM.FFI.Error as Error
import qualified LLVM.FFI.ExecutionEngine as EE
import qualified LLVM.FFI.BitWriter as BW
import qualified LLVM.FFI.Core as Core
import qualified LLVM.Target.Native as Native

import qualified System.IO as IO
import System.Exit (exitFailure)

import qualified Foreign.Marshal.Array as Array
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.C.String as CStr
import Foreign.C.String (withCString)
import Foreign.C.Types (CFloat)
import Foreign.Storable (peek, sizeOf)
import Foreign.Ptr (Ptr, FunPtr, nullPtr)

import Control.Exception (bracket)
import Control.Monad (when, void)

import Data.Tuple.HT (mapSnd)


vectorSize :: Int
roundName :: String

(vectorSize, roundName) =
   if False
     then (4, "llvm.x86.sse41.round.ps")
     else (8, "llvm.x86.avx.round.ps.256")

type Importer f = FunPtr f -> f

foreign import ccall safe "dynamic" derefFuncPtr :: Importer (Ptr a -> IO ())



die :: String -> IO a
die msg = do
   IO.hPutStrLn IO.stderr msg
   exitFailure

exitFromError :: Ptr CStr.CString -> IO a
exitFromError errorRef =
   bracket (peek errorRef) Alloc.free $ \errorMsg ->
      CStr.peekCString errorMsg >>= die

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
   loaded <- withCString "" $ Core.buildLoad2 builder vectorType param
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
               withCString "" $
                  Core.buildCall2 builder funcType roundFunc ps len
            Core.setInstructionCallConv call $
               Core.fromCallingConvention Core.C
            context <- Core.getGlobalContext
            attrKind <-
               CStr.withCStringLen "nofree" $
                  uncurry Core.getEnumAttributeKindForName . mapSnd fromIntegral
            attr <- Core.createEnumAttribute context attrKind 0
            Core.addCallSiteAttribute call Core.attributeFunctionIndex attr
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
      withCString Core.hostTriple $ \triple ->
      (\cont -> do
         target <-
            Alloc.alloca $ \targetRef ->
            Alloc.alloca $ \errorRef -> do
               failure <- TM.getTargetFromTriple triple targetRef errorRef
               if Core.deconsBool failure
                  then exitFromError errorRef
                  else peek targetRef
         cpu <- TM.getHostCPUName
         cont target cpu) $ \target cpu ->

      bracket
         (TM.createTargetMachine target triple cpu nullPtr
            TM.codeGenLevelDefault TM.relocDefault TM.codeModelDefault)
         TM.disposeTargetMachine $
            \tm ->

      bracket PB.createPassBuilderOptions PB.disposePassBuilderOptions $
            \pbOpt ->

      withCString "default<O2>" $ \passName -> do
         PB.passBuilderOptionsSetVerifyEach pbOpt Core.true
         do
            errorRef <- PB.runPasses modul passName tm pbOpt
            when (errorRef /= nullPtr) $
               bracket
                  (Error.getErrorMessage errorRef)
                  Error.disposeErrorMessage
                     $ \errorMsg ->
                  CStr.peekCString errorMsg >>= die
         withCString "round-avx-opt.s" $ \path ->
            Alloc.alloca $ \errorRef -> do
               failure <-
                  TM.targetMachineEmitToFile
                     tm modul path TM.assemblyFile errorRef
               when (Core.deconsBool failure) $
                  exitFromError errorRef

         void $ withCString "round-avx-opt.bc" $ BW.writeBitcodeToFile modul

   bracket (createExecutionEngine modul) EE.disposeExecutionEngine $
         \execEngine -> do
      let vector = take vectorSize $ iterate (1+) (-1.3 :: CFloat)
      funcPtr <- EE.getPointerToFunction execEngine func
      let size = sum $ map sizeOf vector
      Alloc.allocaBytesAligned size size $ \ptr -> do
         Array.pokeArray ptr vector
         derefFuncPtr funcPtr ptr
         print =<< Array.peekArray vectorSize ptr
