module CommonCLI where

import System.IO
import System.FilePath
import System.Directory
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import AnalyzeInstant (compilerFronted, ProgramInstant)


compilerMain :: (String -> String -> IO ()) -> IO ()
compilerMain compile = do
  args <- getArgs
  case args of
    [filepath] -> readFile filepath >>= compile filepath >> exitSuccess
    _ -> usage
    
    
compileToText :: (String -> ProgramInstant -> [String]) -> String -> String -> String -> IO ()
compileToText compiler filename outfile content = case compilerFronted content of
  Left err -> print err >> exitFailure
  Right ast -> do
    handle <- openFile outfile WriteMode
    mapM_ (hPutStrLn handle) $ compiler filename ast
    hClose handle


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "(file)          Compile specified file"
    ]
  exitFailure
