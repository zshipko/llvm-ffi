module Main (main) where

import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix ((=~~))
import Control.Monad (forM_)
import Data.Maybe (catMaybes)


maybeName :: C.ByteString -> Maybe C.ByteString
maybeName line = do
  ((_:name:_):_) <- line =~~ "^[ \t]*([a-z0-9_]+),[ \t]*//[ \t]*llvm\\."
  return name

main :: IO ()
main = do
  input <- (catMaybes . map maybeName . C.lines) `fmap` C.getContents

  putStrLn "-- automatically generated file - do not edit!"
  putStrLn "module LLVM.Core.Intrinsics (Intrinsic(..)) where"
  putStrLn "data Intrinsic ="
  putStrLn "      NotIntrinsic"
  forM_ input $ C.putStrLn . (C.append (C.pack "    | I_"))
  putStrLn "    deriving (Eq, Ord, Enum, Show)"
