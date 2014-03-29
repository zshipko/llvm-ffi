module Main (main) where

import FunctionMangulation (pattern, rewriteFunction)

import Text.Regex.Posix ((=~))

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)

import Control.Monad (forM_)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


cFunctions :: String -> Map String String
cFunctions s =
   let f (_:ret:name:params:_) =
            Just ("LLVM" ++ name, rewriteFunction ret name params)
       f _ = Nothing
   in  Map.fromList $ mapMaybe f (s =~ pattern)

hsFunctions :: String -> Map String String
hsFunctions s =
   let pat = "\"([a-zA-Z0-9_]+)\"[ \t\n]+([a-zA-Z0-9_']+)"
       f (_:cname:hsname:_) = Just (cname, hsname)
       f _ = Nothing
   in  Map.fromList $ mapMaybe f (s =~ pat)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cFile, hsFile] -> do
              c <- cFunctions `fmap` readFile cFile
              hs <- hsFunctions `fmap` readFile hsFile
              putStrLn "In C, not Haskell:"
              forM_ (Map.toAscList $ Map.difference c hs) $ \(_, hsfunc) ->
                    putStrLn hsfunc
              putStrLn "In Haskell, not C:"
              forM_ (Map.keys $ Map.difference hs c) $ putStrLn . ("  "++)
    _ -> do
         hPutStrLn stderr "Usage: DiffFFI cFile hsFile"
         exitFailure
