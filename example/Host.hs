{- |
This program shows some interesting facts about the host machine.
-}
module Main where

import Common (getString)

import qualified LLVM.FFI.Support.Host as Host
import qualified LLVM.FFI.Core as LLVM
import qualified LLVM.Target.Native as Native


main :: IO ()
main = do
   Native.initializeNativeTarget

   putStrLn =<< getString Host.getHostCPUName
   putStrLn LLVM.hostTriple
