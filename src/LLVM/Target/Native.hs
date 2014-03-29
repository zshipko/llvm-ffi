{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.Native(initializeNativeTarget) where

import qualified Foreign.C.Types as C

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Monad (when)

import System.IO.Unsafe (unsafePerformIO)


foreign import ccall unsafe "LLVMInitNativeTarget"
        llvmInitializeNativeTarget :: IO C.CUInt

-- | Initialize jitter to the native target.
-- The operation is idempotent.
initializeNativeTarget :: IO ()
initializeNativeTarget = do
    done <- takeMVar refDone
    when (not done) (llvmInitializeNativeTarget >> return ()) -- initializeTarget
    putMVar refDone True

-- UNSAFE: global variable to keep track of initialization state.
refDone :: MVar Bool
refDone = unsafePerformIO $ newMVar False
