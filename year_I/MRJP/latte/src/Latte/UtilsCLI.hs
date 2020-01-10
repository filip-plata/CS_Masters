module Latte.UtilsCLI where

import System.Exit ( exitFailure )

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "(file)          Compile specified file"
    , "()              Compile from stdin"
    ]
  exitFailure
