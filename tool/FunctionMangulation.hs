module FunctionMangulation
    (
      pattern
    , rewrite
    , rewriteFunction
    ) where

import Text.Regex.Posix ((=~), (=~~))

import Control.Monad (forM)
import Data.Char (toLower)
import Data.List.HT (maybePrefixOf)
import Data.String.HT (trim)
import Data.List (intercalate)


pattern :: String
pattern = "^([A-Za-z0-9_ ]+ ?\\*?)[ \t\n]*" ++
          "LLVM([A-Za-z0-9_]+)\\(([a-zA-Z0-9_*, \t\n]+)\\);"

renameType :: String -> String
renameType t =
   case maybePrefixOf "LLVM" t of
      Just suffix -> rename suffix
      Nothing -> rename t

rename :: String -> String
rename cname =
   case cname of
      "int" -> "CInt"
      "unsigned" -> "CUInt"
      "long long" -> "CLLong"
      "unsigned long long" -> "CULLong"
      "void" -> "()"
      "const char *" -> "CString"
      "char *" -> "CString"
      _ ->
         case reverse cname of
            '*':ps -> "(Ptr " ++ rename (reverse ps) ++ ")"
            _ -> trim cname

split :: (a -> Bool) -> [a] -> [[a]]
split p xs =
   case break p xs of
      (h,(_:t)) -> h : split p t
      (s,_) -> [s]

dropName :: String -> String
dropName s =
   case s =~ "^((const )?[A-Za-z0-9_]+( \\*+)?) ?[A-Za-z0-9]*$" of
      ((_:typ:_):_) -> typ
      _ -> "{- oops! -} " ++ s

rewriteFunction :: String -> String -> String -> String
rewriteFunction cret cname cparams =
    let ret = "IO " ++ renameType (trim cret)
        params = map renameParam . split (==',') $ cparams
	params' = if params == ["()"] then [] else params
        name = let (n:ame) = cname in toLower n : ame
    in foreign ++ "\"LLVM" ++ cname ++ "\" " ++ name ++
           "\n    :: " ++ intercalate " -> " (params' ++ [ret])
  where renameParam = renameType . dropName . trim
        foreign = "foreign import ccall unsafe "

rewrite :: Monad m => String -> m [String]
rewrite s = do
    matches <- s =~~ pattern
    forM matches $ \(_:cret:cname:cparams:_) ->
         return (rewriteFunction cret cname cparams)
