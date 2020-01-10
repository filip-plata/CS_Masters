module Main where


import System.IO
import System.Process (callProcess)
import System.FilePath
import System.Directory
import CompileJVMInstant ( InstructionJVM(..), compileIntermediate )
import AnalyzeInstant ( compilerFronted, stackSize )
import CommonCLI ( compilerMain, compileToText )


main = compilerMain compileJava


compileJava :: String -> String -> IO ()
compileJava filepath content =
  let jasminFile = replaceExtension filepath "j" in
  do
    compileJasmin (takeBaseName filepath) jasminFile content
    jasmin <- jasminPath
    callProcess "java" ["-jar", jasmin, "-d", takeDirectory filepath, jasminFile]


compileJasmin :: String -> String -> String -> IO ()
compileJasmin = compileToText (\name p -> concatMap emitJasmin $ compileIntermediate name p)


jasminPath :: IO FilePath
jasminPath = getCurrentDirectory >>= (\dir -> return $ joinPath [dir, "lib/", "jasmin.jar"])
  
emitJasmin :: InstructionJVM -> [String]
emitJasmin (Store 0) = ["istore_0"]
emitJasmin (Store 1) = ["istore_1"]
emitJasmin (Store 2) = ["istore_2"]
emitJasmin (Store 3) = ["istore_3"]
emitJasmin (Store n) = ["istore " ++ show n]

emitJasmin (Load 0) = ["iload_0"]
emitJasmin (Load 1) = ["iload_1"]
emitJasmin (Load 2) = ["iload_2"]
emitJasmin (Load 3) = ["iload_3"]
emitJasmin (Load n) = ["iload " ++ show n]

emitJasmin (Push 0) = ["iconst_0"]
emitJasmin (Push 1) = ["iconst_1"]
emitJasmin (Push 2) = ["iconst_2"]
emitJasmin (Push 3) = ["iconst_3"]
emitJasmin (Push 4) = ["iconst_4"]
emitJasmin (Push 5) = ["iconst_5"]
emitJasmin (Push n)
  | 0 <= n && n < 128 = ["bipush " ++ show n]
  | 128 <= n && n < 32767 = ["sipush " ++ show n]
  | otherwise = ["ldc " ++ show n]

emitJasmin Swap = ["swap"]

emitJasmin Add = ["iadd"]
emitJasmin Sub = ["isub"]
emitJasmin Div = ["idiv"]
emitJasmin Mul = ["imul"]

emitJasmin PrintInt = ["invokevirtual java/io/PrintStream/println(I)V"]
emitJasmin LoadOut = ["getstatic java/lang/System/out Ljava/io/PrintStream;"]

emitJasmin (Preambule name stackS localsSize) =
  [ ".class public " ++ name
  , ".super java/lang/Object"
  , ".method public <init>()V"
  , "aload_0"
  , "invokenonvirtual java/lang/Object/<init>()V"
  , "return"
  , ".end method"
  , ""
  , ".method public static main([Ljava/lang/String;)V"
  , ".limit stack " ++ show (stackSize stackS)
  , ".limit locals " ++ show (localsSize + 1) -- adding one for args argument
  , ""
  ]
emitJasmin Postambule = ["", "return", ".end method"]
