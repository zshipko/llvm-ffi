module Main (main) where

import FunctionMangulation (rewrite)

import Data.List (intercalate)


main :: IO ()
main = interact (intercalate "\n\n" . concat . rewrite) >> putStr "\n"
