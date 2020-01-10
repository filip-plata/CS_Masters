module Latte.Common.Runtime where

import Latte.Common.AST (Ident(..), Type(..))

arrayDefName, selfRefName :: String
arrayDefName = "__array"
selfRefName = "self"


runtimeDefinitions :: [(Ident, Type)]
runtimeDefinitions = [
    (Ident "printInt", Fun Void [Int])
  , (Ident "printString", Fun Void [Str])
  , (Ident "error", Fun Void [])
  , (Ident "readInt", Fun Int [])
  , (Ident "readString", Fun Str [])
  , (Ident "__concat_str", Fun Str [Str, Str])
  , (Ident "__malloc", Fun Str [Int, Int])
  , (Ident arrayDefName, ClassType (Ident arrayDefName) [(Ident "data", Str), (Ident "length", Int)] [] Nothing)
  ]
