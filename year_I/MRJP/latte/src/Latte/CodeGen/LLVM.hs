{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

module Latte.CodeGen.LLVM where

import System.IO ( Handle )

import Control.Monad.Identity
import Control.Monad (void)
import Control.Monad.State.Lazy (State, gets, put, evalState, get)

import qualified LLVM.AST as LLVM_AST
import qualified LLVM.AST.Type as LLVM_T
import LLVM.AST.Global
import LLVM.IRBuilder.Monad
import qualified LLVM.IRBuilder.Instruction as Instr
import LLVM.IRBuilder.Constant
import LLVM.AST.IntegerPredicate

import Latte.Common.AST
import Latte.Common.Runtime
import Latte.Common.Annotation
import Data.ByteString (ByteString, hPut)
import Data.Map (Map)
import qualified Data.Map as Map
import LLVM.IRBuilder.Module (ModuleBuilderT, function, ParameterName(..), buildModuleT, global, emitDefn, typedef)
import Data.ByteString.Short (toShort, ShortByteString)
import qualified Data.ByteString.Char8 as B

import LLVM.IRBuilder.Instruction (call, bitcast)
import LLVM.AST.Operand (Operand(..))
import qualified LLVM.AST.Constant as LLVM_Constant
import LLVM.AST.Name (mkName)
import Latte.Frontend (LatteProgram)
import Latte.Common.SymbolTable
import Latte.CodeGen.OptimizerLLVM
import Control.Monad.Reader (ReaderT, runReaderT, asks, ask)
import Data.Char (ord)
import LLVM.AST.Type (elementType, nArrayElements)
import LLVM.AST.Typed (typeOf)

import Data.Maybe (fromJust)
import Data.Word (Word32)

import LLVM.Pretty (ppllvm)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)


compileToHandle :: Handle -> String -> LatteProgram -> IO ()
compileToHandle handle sourceName p = hPut handle $ compileToMemory sourceName p


compileToMemory :: String -> LatteProgram -> ByteString
compileToMemory sourceName p =
  let compiledModule = optimize $ compileProgram sourceName p in
  encodeUtf8 $ toStrict $ ppllvm compiledModule

type Operands = Map SymbolName LLVM_AST.Operand

data CompilerState = CompilerState {
  operands :: Operands,
  vTables :: Map SymbolName LLVM_AST.Type,
  fRetType :: Maybe LLVM_T.Type
}

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState Map.empty Map.empty Nothing


type CompilerModuleLLVM = ModuleBuilderT (ReaderT SymbolTable (State CompilerState))


compileProgram :: String -> LatteProgram -> LLVM_AST.Module
compileProgram sourceName (p, st) = evalState (runReaderT (buildModuleT (toShort $ B.pack sourceName) $ compileProgramToModule p) st) emptyCompilerState


compileProgramToModule :: ProgramType -> CompilerModuleLLVM ()
compileProgramToModule (Program topDefs _) = do
  declareRuntime
  defineGlobalStrings
  defineVTables
  defineClassStructTypes
  mapM_ compileTopDef topDefs


compileTopDef :: TopDefType -> CompilerModuleLLVM ()

compileTopDef (GlobalFunDef fDef) = compileFunctionDef fDef

compileTopDef (ClassDef name_ _ _ methods _) =
  mapM_ (\(FunctionDef rType ident args block_ a)
            -> compileFunctionDef $ FunctionDef rType (Ident $ classMethodToLLVMName name_ ident)
                                      args block_ a) methods


compileFunctionDef :: FunDefType -> CompilerModuleLLVM ()
compileFunctionDef (FunctionDef rType (Ident ident) args block_ _) =
    void $ function (LLVM_AST.mkName ident) argsLLVM retT $ buildFunction retT (map (\(Arg _ _ (TypecheckerAnnotation _ _ (Just sym))) -> sym) args) block_
    where retT = compileTypes rType
          argsLLVM = map (\(Arg t (Ident i) _) -> (compileTypes t, ParameterName $ toBS i)) args


type CompilerLLVM = IRBuilderT CompilerModuleLLVM


buildFunction :: LLVM_T.Type -> [SymbolName] -> BlockType -> [LLVM_AST.Operand] -> CompilerLLVM ()

buildFunction fRetT symbols block_ ops = do
  st <- get
  let operands_ = operands st
  put $ st { operands = Map.union operands_ $ Map.fromList $ zip symbols ops, fRetType = Just fRetT }
  emitBlockStart $ mkName "entry"
  compileBlock block_
  put st


compileBlock :: BlockType -> CompilerLLVM ()

compileBlock (Block stmts _) = mapM_ compileStmt stmts


compileStmt :: StmtType -> CompilerLLVM ()

compileStmt (BStmt block_ _) = compileBlock block_

compileStmt (Decl t items _) = mapM_ (compileStmt . compileItem t) items
compileStmt (Ass lexpr@(EVar _ a) rexpr _) = do
  e <- compileExp rexpr
  ops <- gets operands
  case typecheckerToSymbol a of
    Just sym -> do
                  st <- get
                  let lType = typecheckerToType a
                  let rType = typecheckerToType $ annotationExpr rexpr
                  e_ <- if lType /= rType
                          then namedCustom $ Instr.bitcast e (compileTypes lType)
                          else return e
                  put $ st { operands = Map.insert sym e_ ops }
    _ -> error $ "Compiler internal error: identifier has no symbol annotation? " ++ show lexpr

compileStmt (Ass (ESubscript arrayExpr idxExpr _) rexpr _) = do
  arrayOp <- compileExp arrayExpr
  arrayIdx <- compileExp idxExpr
  let Array insideType = typecheckerToType $ annotationExpr arrayExpr
  
  dataPtrPtr <- namedCustom $ Instr.gep arrayOp [intConstant 32 0, intConstant 32 0]
  dataPtr <- namedCustom $ Instr.load dataPtrPtr 0
  targetLoc <- namedCustom $ Instr.gep dataPtr [arrayIdx]
  valOp_ <- compileExp rexpr
  valOp <- namedCustom $ Instr.bitcast valOp_ $ compileTypes insideType
  Instr.store targetLoc 0 valOp

compileStmt (Ass (EAttrRef lexpr attr _) rexpr _) = let symName = symbolForExpr lexpr in do
  symTable <- ask
  case symbolFieldOffset symTable symName attr of
    Just off -> do
      exprPtr <- compileExp lexpr
      rOp <- compileExp rexpr
      targetLoc <- namedCustom $ Instr.gep exprPtr [intConstant 32 0, intConstant 32 off]
      Instr.store targetLoc 0 rOp
    _ -> error "Invalid field for assignment?"

compileStmt (Ret expr _) = do
  retType <- fromJust <$> gets fRetType
  rOp <- compileExp expr
  rOp_ <- namedCustom $ Instr.bitcast rOp retType
  Instr.ret rOp_

compileStmt (VRet _) = Instr.retVoid

compileStmt (CondElse (Not e1 _) sT sF a) =
  compileStmt $ CondElse e1 sF sT a

compileStmt (CondElse expr stmtTrue stmtFalse _) = do
  ifBlock <- fresh `named` toBS "if"
  elseBlock <- fresh `named` toBS "else"
  continueBlock <- fresh `named` toBS "continue"
  exprCom <- compileExp expr
  Instr.condBr exprCom ifBlock elseBlock
  beforeBranchesState <- get
  emitBlockStart ifBlock
  compileStmt stmtTrue
  termIf <- hasTerminator
  leaveIf <- currentBlock
  unless termIf $ Instr.br continueBlock
  afterIfState <- get
  put beforeBranchesState
  emitBlockStart elseBlock
  compileStmt stmtFalse
  termElse <- hasTerminator
  leaveElse <- currentBlock
  unless termElse $ Instr.br continueBlock
  afterElseState <- get
  put beforeBranchesState
  if termElse && termIf
    then return ()
    else do
      emitBlockStart continueBlock
      when (not termElse && not termIf) $
        compilePhiChoice (symbolsForState beforeBranchesState) (afterIfState, leaveIf) (afterElseState, leaveElse)

compileStmt (While expr stmt _) = compileWhileLoop expr stmt

compileStmt (SExp expr _) = void $ compileExp expr

compileStmt (Empty _) = return ()

compileStmt _ = error "Compiling a statement to llvm that fronted ought to decompile"


compileWhileLoop :: ExprType -> StmtType -> IRBuilderT CompilerModuleLLVM ()
compileWhileLoop expr stmt = mdo
   let TypecheckerAnnotation _ doesNotRet _ = annotationStmt stmt
   exprCheck <- fresh `named` toBS "condition"
   loopBody <- fresh `named` toBS "loop"
   continueBlock <- fresh `named` toBS "continue"

   ensureBlock
   entryBlock <- currentBlock
   entryState <- get
   let symbols = symbolsForState entryState

   Instr.br exprCheck

   -- Expr check
   emitBlockStart exprCheck
   mapM_ (\symbol -> emitPhi symbol entryState leaveState entryBlock leaveLoopBlock) symbols
   rememberSt <- get
   expCom <- compileExp expr
   if doesNotRet then Instr.condBr expCom loopBody continueBlock else Instr.br loopBody

   emitBlockStart loopBody
   compileStmt stmt
   term <- hasTerminator
   leaveLoopBlock <- currentBlock
   unless term $ Instr.br exprCheck

   leaveState <- get
   put rememberSt

   if term then return () else emitBlockStart continueBlock


operandForSymbolUnsafe :: Operands -> SymbolName -> Operand
operandForSymbolUnsafe ops sym = fromJust (Map.lookup sym ops)


emitPhi :: SymbolName -> CompilerState -> CompilerState -> LLVM_AST.Name -> LLVM_AST.Name -> CompilerLLVM ()
emitPhi symName entryState leaveState entryBlock loopBody =
  let opEntry = operandForSymbolUnsafe (operands entryState) symName
      opLeave = operandForSymbolUnsafe (operands leaveState) symName in
  namedCustom $ Instr.phi [
   (opEntry, entryBlock),
   (opLeave, loopBody)] >>= updateOperand symName


compilePhiChoice :: [SymbolName] -> (CompilerState, LLVM_AST.Name) -> (CompilerState, LLVM_AST.Name) -> CompilerLLVM ()
compilePhiChoice symbols (state1, l1) (state2, l2) =
  let [ops1, ops2] = map operands [state1, state2]
   in mapM_ (phiSym (ops1, l1) (ops2, l2)) symbols


phiSym :: (Operands, LLVM_AST.Name) -> (Operands, LLVM_AST.Name) -> SymbolName -> CompilerLLVM ()
phiSym (ops1, l1) (ops2, l2) sym =
  let [Just op1, Just op2] = map (Map.lookup sym) [ops1, ops2]
   in when (op1 /= op2) $
      namedCustom $ Instr.phi [(op1, l1), (op2, l2)] >>= \op ->
        get >>= \st -> put $ st {operands = Map.insert sym op (operands st)}


symbolsForState :: CompilerState -> [SymbolName]
symbolsForState state = filter ("__empty_string" /=) $ Map.keys $ operands state

    
updateOperand :: SymbolName -> LLVM_AST.Operand -> CompilerLLVM ()
updateOperand symName op = do
  st <- get
  put $ st { operands = Map.insert symName op (operands st) }


compileExp :: ExprType -> CompilerLLVM LLVM_AST.Operand

compileExp (EVar ident a) =
  case typecheckerToSymbol a of
    Just sym -> do
                  opM <- gets (Map.lookup sym . operands)
                  case opM of
                    Just op -> return op
                    _ -> error $ "Compiler internal error: variable not an operand? " ++ show ident
    Nothing -> error "Variable without type in backend"

compileExp (ELitInt n _) = defaultIntType n

compileExp (ELitTrue _) = bit 1

compileExp (ELitFalse _) = bit 0

compileExp (EApp fun args _) = compileExpApp fun args

compileExp (EString _ a) =
  case typecheckerToSymbol a of
    Just sym -> do
      opM <- gets (Map.lookup sym . operands)
      case opM of
        Just op -> namedCustom $ bitcast op $ LLVM_T.ptr LLVM_T.i8
        _ -> error "Compiler internal error: global string constant not an operand?"
    _ -> error "Compiler internal error: string has no symbol annotation?"

compileExp (Neg expr _) = do
  operand1 <- compileExp expr
  minusOne <- minusOneM
  namedCustom $ Instr.mul minusOne operand1

compileExp (Not expr _) = do
  operand1 <- compileExp expr
  one <- bit 1
  namedCustom $ Instr.xor one operand1

compileExp (EMul expr1 mulSign expr2 _) = do
  operand1 <- compileExp expr1
  operand2 <- compileExp expr2
  namedCustom $ case mulSign of
    Times -> Instr.mul operand1 operand2
    Div -> Instr.sdiv operand1 operand2
    Mod -> emitInstr (typeOf operand1) $ LLVM_AST.SRem operand1 operand2 []

compileExp (EAdd expr1 addSign expr2 _) = do
  operand1 <- compileExp expr1
  operand2 <- compileExp expr2
  namedCustom $ case addSign of
    Plus -> Instr.add operand1 operand2
    Minus -> Instr.sub operand1 operand2
    
compileExp (EConcat expr1 expr2 a) = compileExp $ EApp (EVar (Ident "__concat_str") a) [expr1, expr2] a

compileExp (ERel expr1 relSign expr2 _) = do
  operand1 <- compileExp expr1
  operand2 <- compileExp expr2
  namedCustom $ case relSign of
    LTH -> Instr.icmp SLT operand1 operand2
    LE -> Instr.icmp SLE operand1 operand2
    GTH -> Instr.icmp SGT operand1 operand2
    GE -> Instr.icmp SGE operand1 operand2
    EQU -> do
      op2_t <- namedCustom $ Instr.bitcast operand2 $ typeOf operand1
      namedCustom $ Instr.icmp LLVM.AST.IntegerPredicate.EQ operand1 op2_t
    Latte.Common.AST.NE -> do
      op2_t <- namedCustom $ Instr.bitcast operand2 $ typeOf operand1
      namedCustom $ Instr.icmp LLVM.AST.IntegerPredicate.NE operand1 op2_t

compileExp (EAnd expr1 expr2 _) = do
  middle <- fresh `named` toBS "short_circuit"
  continue <- fresh `named` toBS "short_circuit_cont"
  false <- bit 0

  operand1 <- compileExp expr1
  leaveOp1 <- currentBlock
  Instr.condBr operand1 middle continue

  emitBlockStart middle
  operand2 <- compileExp expr2
  leaveOp2 <- currentBlock
  Instr.br continue

  emitBlockStart continue
  namedCustom $ Instr.phi [(false, leaveOp1), (operand2, leaveOp2)]

compileExp (EOr expr1 expr2 _) = do
  middle <- fresh `named` toBS "short_circuit"
  continue <- fresh `named` toBS "short_circuit_cont"
  true <- bit 1

  operand1 <- compileExp expr1
  leaveOp1 <- currentBlock
  Instr.condBr operand1 continue middle

  emitBlockStart middle
  operand2 <- compileExp expr2
  leaveOp2 <- currentBlock
  Instr.br continue

  emitBlockStart continue
  namedCustom $ Instr.phi [(true, leaveOp1), (operand2, leaveOp2)]

compileExp (EObjNew classIdent@(Ident cIdent) _) = let malloc = "__malloc" in do
  fSymbol <- asks (resolveSymbol malloc)

  case fSymbol of
    Just sym -> do
      classStorageSize <- typeStorageSize $ Class classIdent
      vTable <- gets vTables
      let vTableType = fromJust $ Map.lookup cIdent vTable
      rawPtr <- namedCustom $ call (mallocF sym) $ map (, []) [intConstant 32 1, classStorageSize]
      vTablePtrPtr <- namedCustom $ Instr.bitcast rawPtr $ LLVM_T.ptr $ LLVM_T.ptr vTableType

      Instr.store vTablePtrPtr 0 (ConstantOperand
                    (LLVM_Constant.GlobalReference
                       (LLVM_T.ptr vTableType)
                       (mkName $ classVTableLLVMName cIdent)))

      namedCustom $ Instr.bitcast rawPtr $ compileTypes (Class classIdent)
    Nothing -> error "No __malloc symbol provided!"

compileExp (EArrayNew arrayType expr _) = let malloc = "__malloc" in do
  fSymbol <- asks (resolveSymbol malloc)
  arrayLength <- compileExp expr
  let llvmType = compileTypes arrayType
  let llvmPtrType = LLVM_T.ptr llvmType
  arrayStorageSize <- typeStorageSize (Array arrayType)
  insideTypeStorageSize <- typeStorageSize arrayType
  
  case fSymbol of
    Just sym -> do
      rawPtr <- namedCustom $ call (mallocF sym) $ map (, []) [arrayLength, insideTypeStorageSize]
      dataPtr <- namedCustom $ Instr.bitcast rawPtr llvmPtrType

      arrayPtr <- namedCustom $ call (mallocF sym) $ map (, []) [intConstant 32 1, arrayStorageSize]
      arrayStructPtr <- namedCustom $ Instr.bitcast arrayPtr $ compileTypes (Array arrayType)
      arrayDataPtr <- namedCustom $ Instr.gep arrayStructPtr [intConstant 32 0, intConstant 32 0]

      Instr.store arrayDataPtr 0 dataPtr
      arrayLengthPtr <- namedCustom $ Instr.gep arrayStructPtr [intConstant 32 0, intConstant 32 1]
      Instr.store arrayLengthPtr 0 arrayLength

      return arrayStructPtr
    Nothing -> error "No __malloc symbol provided!"


compileExp (ESubscript exprArray exprIdx _) = do
  arrayOp <- compileExp exprArray
  arrayIdx <- compileExp exprIdx
  
  dataPtrPtr <- namedCustom $ Instr.gep arrayOp [intConstant 32 0, intConstant 32 0]
  dataPtr <- namedCustom $ Instr.load dataPtrPtr 0
  dataLoc <- namedCustom $ Instr.gep dataPtr [arrayIdx]
  namedCustom $ Instr.load dataLoc 0

compileExp (EAttrRef expr attr _) = let symName = symbolForExpr expr in do
  symTable <- ask
  exprPtr <- compileExp expr
  vTable <- gets vTables

  case symbolFieldOffset symTable symName attr of
    Just off -> namedCustom $ Instr.gep exprPtr [intConstant 32 0, intConstant 32 off] >>= (namedCustom . (`Instr.load` 0))
    Nothing ->
      case methodVTableOffset symTable symName  attr of
        VirtualOffset idx -> let vTableType = fromJust $ Map.lookup symName vTable in do
          vTablePtrPtr <- namedCustom $ Instr.bitcast exprPtr $ LLVM_T.ptr $ LLVM_T.ptr vTableType
          vTablePtr <- namedCustom $ Instr.load vTablePtrPtr 0
          methodPtr <- namedCustom $ Instr.gep vTablePtr [intConstant 32 0, intConstant 32 idx]
          namedCustom $ Instr.load methodPtr 0

        NonVirtualOrigin origClass ->
          let methodType = fromJust $ getClassMethodType symTable origClass attr in
          return $ ConstantOperand $
                     LLVM_Constant.GlobalReference
                       (LLVM_T.ptr $ compileTypes methodType)
                       (mkName $ classMethodToLLVMName (Ident origClass) attr)

        Unknown -> error $ "Could not find method " ++ show attr ++ " in class " ++ show symName

compileExp (ENull t _) = return $ ConstantOperand $ LLVM_Constant.Null $ compileTypes t

compileExp (ESelf a) = compileExp $ EVar (Ident selfRefName) a


mallocF :: Symbol -> Operand
mallocF sym = ConstantOperand
                 (LLVM_Constant.GlobalReference
                    (LLVM_T.ptr $ compileTypes $ getSymbolType sym)
                    (mkName $ getSymbolName sym))

  
compileExpApp :: ExprType -> [ExprType] -> CompilerLLVM LLVM_AST.Operand

compileExpApp (EVar (Ident ident) _) args = do
  ops <- mapM compileExp args
  fType <- asks (resolveSymbol ident)
  case fType of
    Just t ->
      let fRef =
            ConstantOperand (LLVM_Constant.GlobalReference (LLVM_T.ptr $ compileTypes $ getSymbolType t) (mkName ident))
          argTypes = unsafeClassArgType fRef
       in castCallArgs ops argTypes >>= \callOps ->  namedCustom $ call fRef $ map (, []) callOps
    Nothing -> error $ "No global function with name: " ++ ident

compileExpApp eRef @ (EAttrRef classObj _ _ ) args = do
  classObj_ <- compileExp classObj
  ops <- mapM compileExp args
  fRef <- compileExp eRef
  let argTypes = unsafeClassArgType fRef
  callOps <- castCallArgs (classObj_ : ops) argTypes
  namedCustom $ call fRef $ map (, []) callOps

compileExpApp _ _ = error "Unknown application pattern"


unsafeClassArgType :: Operand -> [LLVM_T.Type]
unsafeClassArgType (LLVM_AST.LocalReference
  (LLVM_T.PointerType (LLVM_T.FunctionType _ argsT _) _) _) = argsT
unsafeClassArgType (ConstantOperand
  (LLVM_Constant.GlobalReference (LLVM_T.PointerType (LLVM_T.FunctionType _ argsT _) _) _)) = argsT
unsafeClassArgType _ = error "Unsafe function failed ?!"


castCallArgs :: [Operand] -> [LLVM_T.Type] -> CompilerLLVM [Operand]
castCallArgs = zipWithM (\x y -> namedCustom $ Instr.bitcast x y)


compileItem :: Type -> ItemType -> StmtType
compileItem t (NoInit ident a) = Ass (EVar ident a) (defaultInitializer a t) a
compileItem _ (Init ident expr a) = Ass (EVar ident a) expr a


minusOneM :: CompilerLLVM LLVM_AST.Operand
minusOneM = defaultIntType (-1)


oneM :: CompilerLLVM LLVM_AST.Operand
oneM = defaultIntType 1


defaultInitializer :: TypecheckerAnnotation -> Type -> ExprType
defaultInitializer a Int = ELitInt 0 a
defaultInitializer a Bool = ELitFalse a
defaultInitializer _ Str = EString "" $ TypecheckerAnnotation Str True (Just "__empty_string")
defaultInitializer a t@(Class _) = ENull t a
defaultInitializer a t@(Array _) = ENull t a
defaultInitializer _ _ = error "No default initializer specified"


defaultIntType :: Applicative f => Integer -> f LLVM_AST.Operand
defaultIntType = int32


compileTypes :: Latte.Common.AST.Type -> LLVM_AST.Type
compileTypes Int = LLVM_T.i32
compileTypes Str = LLVM_T.ptr LLVM_T.i8
compileTypes Bool =  LLVM_T.i1
compileTypes Void = LLVM_AST.VoidType
compileTypes (Fun rType argsTy) = LLVM_AST.FunctionType {
  LLVM_AST.resultType = compileTypes rType,
  LLVM_AST.argumentTypes = map compileTypes argsTy,
  LLVM_AST.isVarArg = False
}
compileTypes (Array t) = LLVM_T.ptr $ LLVM_T.StructureType False
  [LLVM_T.ptr $ compileTypes t, LLVM_T.i32] -- first data then length
compileTypes (Class classIdent) =
  LLVM_T.ptr $ LLVM_AST.NamedTypeReference $
    LLVM_AST.mkName $ classNameToLLVMName classIdent
compileTypes ClassType {} = error "Extended class type does not belong to backend"


typeStorageSize :: Latte.Common.AST.Type -> CompilerLLVM Operand
typeStorageSize Int = return $ ConstantOperand $ LLVM_Constant.Int 32 4
typeStorageSize Bool = return $ ConstantOperand $ LLVM_Constant.Int 32 1
typeStorageSize (Fun _ _) = return $ ConstantOperand $ LLVM_Constant.Int 32 ptrSizeof
typeStorageSize Str = return $ ConstantOperand $ LLVM_Constant.Int 32 ptrSizeof
typeStorageSize ClassType {} = return $ ConstantOperand $ LLVM_Constant.Int 32 ptrSizeof
typeStorageSize (Class (Ident s)) = do
  symTable <- ask
  case symbolStorageSize symTable (mkSymbolName s) of
    Just size -> return $ intConstant 32 $ toInteger size
    Nothing -> error $ "Cannot obtain storage size for class " ++ s
typeStorageSize (Array _) = return $ ConstantOperand $ LLVM_Constant.Int 32 12
typeStorageSize Void = error "Asking for Void type size ?!"


declareRuntime :: CompilerModuleLLVM ()
declareRuntime = mapM_ declareGlobalDefinition runtimeDefinitions


defineClassStructTypes :: CompilerModuleLLVM ()
defineClassStructTypes = do
  symTable <- ask
  let classSymbols = getClassSymbols symTable
  mapM_ (\symName -> unless (symName == arrayDefName) $ defineClassStructType symName) classSymbols


defineClassStructType :: SymbolName -> CompilerModuleLLVM ()
defineClassStructType symName = do
  symTable <- ask
  vTable <- gets vTables
  let mFields = getClassFields symTable symName
  case mFields of
    Just fields ->
      let classSymbolName = classNameToLLVMName $ Ident symName in
      void $ typedef (LLVM_AST.mkName classSymbolName)
        (Just $ LLVM_AST.StructureType True $
           LLVM_T.ptr (fromJust (Map.lookup symName vTable)) : map (compileTypes . snd) fields)
    Nothing -> error $ "Could not find fields for class: " ++ symName


defineVTables :: CompilerModuleLLVM ()
defineVTables = do
  symTable <- ask
  let classSymbols = getClassSymbols symTable
  mapM_ (\symName -> unless (symName == arrayDefName) $ defineVTable symName) classSymbols


defineVTable :: SymbolName -> CompilerModuleLLVM ()
defineVTable symName = do
  symTable <- ask
  let vTableEntries = fromJust $ virtualMethodsTable symTable symName
  let vTableTypes = map (\(_, _, t) -> t) vTableEntries
  let vTableType = LLVM_AST.StructureType False $ map (LLVM_T.ptr . compileTypes) vTableTypes

  emitDefn $ LLVM_AST.GlobalDefinition $ globalVariableDefaults {
    name = LLVM_AST.mkName $ classVTableLLVMName symName,
    isConstant = True,
    type' = vTableType,
    initializer = Just $ LLVM_Constant.Struct Nothing False $
      map (\(methodSym, methodIdent, methodType) ->  LLVM_Constant.GlobalReference
        (LLVM_T.ptr $ compileTypes methodType)
        (mkName $ classMethodToLLVMName (Ident methodSym) methodIdent)) vTableEntries
  }
  st <- get
  put $ st { vTables = Map.insert symName vTableType (vTables st) }


classMethodToLLVMName :: Ident -> Ident -> String
classMethodToLLVMName (Ident cName) (Ident methodName) = "method." ++ cName ++ "." ++ methodName


classNameToLLVMName :: Ident -> String
classNameToLLVMName (Ident cName) = "class." ++ cName


classVTableLLVMName :: String -> String
classVTableLLVMName cName = "vtable." ++ cName


defineGlobalStrings :: CompilerModuleLLVM ()
defineGlobalStrings = do
  symList <- asks toList
  mapM_ defineGlobalString symList


defineGlobalString :: Symbol -> CompilerModuleLLVM ()
defineGlobalString sym =
  let symName = getSymbolName sym
   in case getSymbolValue sym of
        S str -> do
          let nulledStr = str ++ "\0"
          op <- global (LLVM_AST.mkName symName) (constantStringType nulledStr) $ stringInitializer nulledStr
          ops <- gets operands
          st <- get
          put $ st {operands = Map.insert symName op ops}
        _ -> return ()


stringInitializer :: String -> LLVM_Constant.Constant
stringInitializer str = LLVM_Constant.Array LLVM_T.i8 $ map (LLVM_Constant.Int 8 . toInteger . ord) str


constantStringType :: String -> LLVM_T.Type
constantStringType str = LLVM_T.ArrayType {nArrayElements = fromIntegral $ length str, elementType = LLVM_T.i8}


declareGlobalDefinition :: (Ident, Latte.Common.AST.Type) -> CompilerModuleLLVM ()
declareGlobalDefinition (Ident ident, Fun rType argTypes) =
  void $ function (LLVM_AST.mkName ident)
    (map (\(t, i) -> (compileTypes t, ParameterName $ toBS $ show (i::Integer)))
         (zip argTypes nat)) (compileTypes rType) (\_ -> return ())
declareGlobalDefinition _ = return ()


symbolForExpr :: ExprType -> SymbolName
symbolForExpr e = mkSymbolName $ case typecheckerToType $ annotationExpr e of
  Array _ -> arrayDefName
  Class (Ident ident) -> ident
  _ -> error "Only classes and arrays have symbolic names"


intConstant :: Word32 -> Integer -> Operand
intConstant size val = ConstantOperand $ LLVM_Constant.Int size val


ptrSizeof :: Integer
ptrSizeof = 8

namedCustom
  :: MonadIRBuilder m
  => m r
  -> m r
namedCustom ir = ir `named` toBS "lat"


toBS :: String -> ShortByteString
toBS s = toShort $ B.pack s


nat :: [Integer]
nat = 1 : map succ nat
