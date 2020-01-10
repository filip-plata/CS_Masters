module CompileLLVMInstant where

import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Writer.Lazy (WriterT, tell, execWriterT, appEndo, Endo(Endo))
import qualified Data.Map as Map
import AnalyzeInstant


newtype IdentLLVM = IdentLLVM String

data ValueLLVM = Identifier IdentLLVM | PlainValue Integer | UndefinedValue

data WrapperLLVM = Preambule String | Postambule | PrintInt ValueLLVM

data InstructionLLVM
  = Add ValueLLVM ValueLLVM
  | Sub ValueLLVM ValueLLVM
  | Mul ValueLLVM ValueLLVM
  | Div ValueLLVM ValueLLVM


data AssingmentLLVM = AssingmentLLVM {
    instruction :: InstructionLLVM,
    ident :: IdentLLVM
} | SpecialLLVM WrapperLLVM


data CompilerLLVMState = CompilerLLVMState {
    vars :: Map Ident ValueLLVM,
    usedIdents :: Integer
}

type CompilerLLVM = WriterT (Endo [AssingmentLLVM]) (State CompilerLLVMState)
  
  
compileIntermediate :: String -> ProgramInstant -> [AssingmentLLVM]
compileIntermediate baseName p = evalState (execWriterT (compileProgramLLVM baseName p) >>= (\v -> return $ appEndo v [])) (CompilerLLVMState Map.empty 0)


compileProgramLLVM :: String -> ProgramInstant -> CompilerLLVM ()
compileProgramLLVM baseName (Prog stmts stats) = do
  tell $ Endo ([SpecialLLVM $ Preambule baseName]<>)
  mapM_ compileStmtLLVM stmts
  tell $ Endo ([SpecialLLVM Postambule]<>)


compileStmtLLVM :: StmtInstant -> CompilerLLVM ()
compileStmtLLVM (SExp exp _) =
  do
    val <- compileExpLLVM exp
    tell $ Endo ([SpecialLLVM $ PrintInt val]<>)
compileStmtLLVM (SAss ident exp _) =
  do
    val <- compileExpLLVM exp
    CompilerLLVMState vars idents <- get
    let newVars = Map.insert ident val vars
    put $ CompilerLLVMState newVars idents


compileExpLLVM :: ExpInstant -> CompilerLLVM ValueLLVM
compileExpLLVM (ExpLit n _) = return $ PlainValue n
compileExpLLVM (ExpVar ident _) =
  do
    CompilerLLVMState vars _ <- get
    return $ Map.findWithDefault UndefinedValue ident vars
compileExpLLVM (ExpAdd exp1 exp2 _) = compileBinaryExpLLVM Add exp1 exp2
compileExpLLVM (ExpSub exp1 exp2 _) = compileBinaryExpLLVM Sub exp1 exp2
compileExpLLVM (ExpMul exp1 exp2 _) = compileBinaryExpLLVM Mul exp1 exp2
compileExpLLVM (ExpDiv exp1 exp2 _) = compileBinaryExpLLVM Div exp1 exp2

compileBinaryExpLLVM :: (ValueLLVM -> ValueLLVM -> InstructionLLVM) -> ExpInstant -> ExpInstant
                        -> CompilerLLVM ValueLLVM
compileBinaryExpLLVM f exp1 exp2 =
  do
    val1 <- compileExpLLVM exp1
    val2 <- compileExpLLVM exp2
    inst <- getNextIdent $ f val1 val2
    tell $ Endo ([inst]<>)
    return $ Identifier $ getIdentifier inst


getNextIdent :: InstructionLLVM -> CompilerLLVM AssingmentLLVM
getNextIdent instruction =
  do
    CompilerLLVMState vars idents <- get
    let nextIdent = idents + 1
    put $ CompilerLLVMState vars nextIdent
    return $ AssingmentLLVM instruction $ newIdentifier nextIdent

newIdentifier :: Integer -> IdentLLVM
newIdentifier val = IdentLLVM $ "%var" ++ show val


getIdentifier :: AssingmentLLVM -> IdentLLVM
getIdentifier (AssingmentLLVM _ ident) = ident


toString :: ValueLLVM -> String
toString (PlainValue n) = show n
toString (Identifier (IdentLLVM ident)) = ident
-- UndefinedValue is a programming error at this point
toString UndefinedValue = undefined
