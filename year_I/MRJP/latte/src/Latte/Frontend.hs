module Latte.Frontend (
  compileIntoAST,
  LatteProgram
)
where

import System.IO (Handle, hGetContents, hPutStrLn, stderr)
import System.Exit ( exitFailure )

import Text.Megaparsec.Error (errorBundlePretty)

import Latte.Common.Annotation (ProgramType)
import Latte.Common.SymbolTable
import Latte.Frontend.Parser (parseProgram)
import Latte.Frontend.Typechecker (checkType, latteIssuePretty)


type ProgramSourceDescription = String
type LatteProgram = (ProgramType, SymbolTable)


compileIntoAST :: ProgramSourceDescription -> Handle -> IO LatteProgram
compileIntoAST desc handle = do
    programSource <- hGetContents handle
    let parseRes = parseProgram desc programSource
    case parseRes of
      Left e -> errToStderr >> putStr (errorBundlePretty e) >> exitFailure
      Right ast -> case checkType ast of
                     (Left issue, _) -> errToStderr >> hPutStrLn stderr (latteIssuePretty issue) >> exitFailure
                     (Right typecheckedRepr, st) -> okToStderr >> return (typecheckedRepr, st)

okToStderr, errToStderr :: IO ()
errToStderr = hPutStrLn stderr "ERROR"
okToStderr = hPutStrLn stderr "OK"
