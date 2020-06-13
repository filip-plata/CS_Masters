module Main where

import System.IO
import Parser (parseString)
import SatSolver (satSolver)


main :: IO Int
main = do
  eof <- isEOF
  if eof
    then return 0
    else do
      line <- getLine -- read the input
      let phi = parseString line -- call the parser
      let sat = satSolver phi -- call the sat solver
      if sat
        then putStrLn "1" -- write 1 if the formula is satisfiable
        else putStrLn "0" -- write 0 if the formula is not satisfiable
      return 0
