module Main where

import System.Process (callProcess)
import System.Directory (getCurrentDirectory)
import System.FilePath ( replaceExtension, joinPath )
import System.IO ( stdin, stdout, withFile, IOMode(ReadMode, WriteMode), Handle )
import System.Environment ( getArgs )
import System.Exit ( exitSuccess )

import Latte.Frontend (compileIntoAST)
import Latte.CodeGen.LLVM (compileToHandle)
import Latte.UtilsCLI (usage)


main :: IO ()
main = do
   rPath <- runtimePath
   args <- getArgs
   case args of
     [filepath] -> do
                     let outfile = replaceExtension filepath "ll"
                     let bytefile = replaceExtension filepath "bc"
                     let rBinPath = replaceExtension rPath "bc"
                     let withInFile = withFile filepath ReadMode
                     let withOutFile = withFile outfile WriteMode
                     
                     withInFile (withOutFile . compile filepath)
                     callProcess "llvm-as" [outfile]
                     callProcess "llvm-as" [rPath]
                     callProcess "llvm-link" [bytefile, rBinPath, "-o", bytefile]
                     exitSuccess
     [] -> compile "(stdin)" stdin stdout >> exitSuccess
     _ -> usage
     
     
compile :: String -> Handle -> Handle -> IO ()
compile sourceName handleIn handleOut = compileIntoAST sourceName handleIn >>= compileToHandle handleOut sourceName


runtimePath :: IO FilePath
runtimePath = getCurrentDirectory >>= (\dir -> return $ joinPath [dir, "lib", "latte_runtime.ll"])
