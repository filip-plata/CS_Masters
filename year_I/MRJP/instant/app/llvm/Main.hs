module Main where

import System.FilePath
import System.Directory (getCurrentDirectory)
import Text.Printf

import System.Process (callProcess)
import CommonCLI ( compilerMain, compileToText )
import CompileLLVMInstant


main = compilerMain compileLLVM

compileLLVM :: String -> String -> IO ()
compileLLVM filepath content =
    let llvmTextFile = replaceExtension filepath "ll" in
    let llvmBinFile = replaceExtension filepath "bc" in
    do
      compileLLVMText (takeBaseName filepath) llvmTextFile content
      callProcess "llvm-as" [llvmTextFile]


compileLLVMText :: String -> String -> String -> IO ()
compileLLVMText = compileToText (\name p -> concatMap emitLLVM $ compileIntermediate name p)


emitLLVM :: AssingmentLLVM -> [String]
emitLLVM (SpecialLLVM (Preambule filename)) = [
  printf "source_filename = \"%s\"" filename,
  "declare i32 @printf(i8*, ...)",
  "@s1 = private constant [4 x i8] c\"%d\\0A\\00\"",
  printf "define void @printInt(%s %%n) {" integerType,
  "%s1 = bitcast [4 x i8]*@s1 to i8*",
  printf "call i32 (i8*, ...) @printf(i8* %%s1, %s %%n)" integerType,
  "ret void",
  "}",
  "define i32 @main() {",
  ""
  ]
emitLLVM (SpecialLLVM Postambule) = ["", printf "ret %s 0" integerType, "}"]
emitLLVM (SpecialLLVM (PrintInt val)) = [printf "call void @printInt(%s %s)" integerType $ toString val]
emitLLVM (AssingmentLLVM inst (IdentLLVM var)) = [printf "%s = %s" var $ emitLLVMIns inst]


emitLLVMIns :: InstructionLLVM -> String
emitLLVMIns (Add val1 val2) = emitLLVMInsBinary "add" val1 val2
emitLLVMIns (Sub val1 val2) = emitLLVMInsBinary "sub" val1 val2
emitLLVMIns (Mul val1 val2) = emitLLVMInsBinary "mul" val1 val2
emitLLVMIns (Div val1 val2) = emitLLVMInsBinary "sdiv" val1 val2

emitLLVMInsBinary :: String -> ValueLLVM -> ValueLLVM -> String
emitLLVMInsBinary operand val1 val2 = printf "%s %s %s, %s" operand integerType (toString val1) (toString val2)

integerType :: String
integerType = "i32"
