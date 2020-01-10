module Main where

import System.IO (stdin, withFile, IOMode(ReadMode))
import System.Environment ( getArgs )
import System.Exit ( exitSuccess )

import Latte.Frontend (compileIntoAST)
import Latte.UtilsCLI (usage)


main :: IO ()
main = do
   args <- getArgs
   case args of
     [filepath] -> withFile filepath ReadMode (compileIntoAST filepath) >> exitSuccess
     [] -> compileIntoAST "(stdin)" stdin >>= (\(p, _) -> print p)
     _ -> usage
