module Main where

import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import AnalyzeInstant
import CommonCLI ( compilerMain )
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)


type VarStore = Map Ident Integer
type InterpreterState =  ExceptT String (StateT VarStore IO)


main = compilerMain interpret

interpret :: String -> String -> IO ()
interpret _ content =
  case compilerFronted content of
    Left err -> print err >> exitFailure
    Right prog -> evalStateT (runExceptT $ interpretProgram prog) Map.empty >>= (\res ->
      case res of
        Left err -> putStrLn err >> exitFailure
        Right _ -> exitSuccess)

interpretProgram :: ProgramInstant -> InterpreterState ()
interpretProgram (Prog stmts _) = mapM_ interpretStmt stmts


interpretStmt :: StmtInstant -> InterpreterState ()
interpretStmt (SExp exp _) = do
  val <- interpretExp exp
  liftIO $ print val
interpretStmt (SAss ident exp _) =
  do
    res <- interpretExp exp
    store <- get
    put $ Map.insert ident res store


interpretExp :: ExpInstant -> InterpreterState Integer
interpretExp (ExpLit n _) = return n
interpretExp (ExpVar var _) = 
  do
    store <- get
    let val = Map.lookup var store
    case val of
      Just n -> return n
      Nothing -> throwError $ "Undefined variable" ++ show var
interpretExp (ExpAdd exp1 exp2 _ ) = interpretBinExp (+) exp1 exp2
interpretExp (ExpSub exp1 exp2 _ ) = interpretBinExp (-) exp1 exp2
interpretExp (ExpMul exp1 exp2 _ ) = interpretBinExp (*) exp1 exp2
interpretExp (ExpDiv exp1 exp2 _ ) = interpretBinExp quot exp1 exp2

interpretBinExp :: (Integer -> Integer -> Integer) -> ExpInstant -> ExpInstant -> InterpreterState Integer
interpretBinExp f exp1 exp2 =
  do
    res1 <- interpretExp exp1
    res2 <- interpretExp exp2
    return $ f res1 res2

