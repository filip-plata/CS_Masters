{-# LANGUAGE StandaloneDeriving #-}

module Latte.CodeGen.OptimizerLLVM (
  optimize
) where

import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe, fromJust)

import LLVM.AST
import LLVM.AST.CallingConvention
import LLVM.AST.InlineAssembly
import LLVM.AST.Global (basicBlocks)
import qualified LLVM.AST.Typed as LLVM_T

import Data.Data
import Data.Generics.Uniplate.Data

deriving instance Ord Dialect
deriving instance Ord InlineAssembly
deriving instance Ord CallingConvention
deriving instance Ord Instruction

type RewriteRules t = Map t t


optimize :: Module -> Module
optimize module_ = foldl (flip ($)) module_ $ map functionLevelOpt [rewriteBasedOpts, removeUnneededRegisters]


rewriteBasedOpts :: [BasicBlock] -> [BasicBlock]
rewriteBasedOpts blocks =
  let rewrites = foldl Map.union Map.empty (map ($ blocks) [bitcastToTheSameTypeRemoval, oneSourcePhiRemoval])
      simpleOpts = applyRewrites (commonSubexpressionElimination blocks) (applyRewrites rewrites blocks)
      subExpr =
        foldl (\blocks_ _ -> applyRewrites (commonSubexpressionElimination blocks_) blocks_)
              simpleOpts $ replicate 3 (0 :: Integer)
   in subExpr


functionLevelOpt :: ([BasicBlock] -> [BasicBlock]) -> Module -> Module
functionLevelOpt opt module_ = module_ {
  moduleDefinitions = map (functionLevelOptDefinition opt) $ moduleDefinitions module_
}


functionLevelOptDefinition :: ([BasicBlock] -> [BasicBlock]) -> Definition -> Definition
functionLevelOptDefinition opt def =
  case def of
    GlobalDefinition global ->
      case global of
        f@Function {basicBlocks = b} -> GlobalDefinition $ f {basicBlocks = opt b}
        _ -> def
    _ -> def


bitcastToTheSameTypeRemoval :: [BasicBlock] -> RewriteRules Operand
bitcastToTheSameTypeRemoval blocks =
  let instructions = toInstructionList blocks
      rewrites = Map.fromList $ mapMaybe bitcastRewrite instructions in

  rewrites

  where bitcastRewrite instr =
         case instr of
            resName := BitCast op typeCast _ ->
              let typeRef = LLVM_T.typeOf op in
              if typeRef == typeCast then Just (LocalReference typeRef resName, op) else Nothing
            _ -> Nothing


oneSourcePhiRemoval :: [BasicBlock] -> RewriteRules Operand
oneSourcePhiRemoval blocks =
  let instructions = toInstructionList blocks
      rewrites = Map.fromList $ mapMaybe phiRewrite instructions in

  rewrites

  where phiRewrite instr =
         case instr of
           resName := Phi type_ incoming _ ->
             let phiResOp = LocalReference type_ resName
                 incomingSet = Set.delete phiResOp (Set.fromList $ map fst incoming) in
             case Set.toList incomingSet of
               [op] -> Just (phiResOp, op)
               _ -> Nothing
           _ -> Nothing


commonSubexpressionElimination :: [BasicBlock] -> RewriteRules Name
commonSubexpressionElimination blocks =
  let instructions = map toInstructions blocks
      rewrites = foldl Map.union Map.empty $ map (snd . foldl subExprRewrite (Map.empty, Map.empty)) instructions in

  rewrites

  where subExprRewrite (nameMapping, rules) instr =
          case instr of
            _ := Call {} -> (nameMapping, rules)
            resName := ins ->
              let newMapping = if Map.notMember ins nameMapping
                                 then Map.insert ins resName nameMapping
                                 else nameMapping in
              case Map.lookup ins nameMapping of
                Just opName -> (newMapping, Map.insert resName opName rules)
                Nothing -> (newMapping, rules)
            _ -> (nameMapping, rules)


applyRewrites :: (Data a, Ord a) => RewriteRules a -> [BasicBlock] -> [BasicBlock]
applyRewrites rewrites = map $ applyRewritesBase rewrites


applyRewritesBase :: (Data a, Ord a) => RewriteRules a -> BasicBlock -> BasicBlock
applyRewritesBase rewrites (BasicBlock name instructions term) =
  let rewriteAction ins =
        case ins of
          name_ := ins_ -> name_ := rewriteBi (`Map.lookup` rewrites) ins_
          Do ins_ -> Do $ rewriteBi (`Map.lookup` rewrites) ins_
      rewrittenInstructions = map rewriteAction instructions
      rewrittenTerminator = rewriteAction term in
  BasicBlock name rewrittenInstructions rewrittenTerminator


applyRewritesPhi :: (Data a, Ord a) => RewriteRules a -> BasicBlock -> BasicBlock
applyRewritesPhi rewrites (BasicBlock name instructions term) =
  let rewriteAction ins =
        case ins of
          name_ := ins_@Phi {} -> name_ := rewriteBi (`Map.lookup` rewrites) ins_
          _ -> ins
      rewrittenInstructions = map rewriteAction instructions in
  BasicBlock name rewrittenInstructions term


incomingBlocksCFG :: [BasicBlock] -> Map Name [Name]
incomingBlocksCFG blocks =
  let edges = foldl (\res block@(BasicBlock name_ _ term) -> (incomingEdges name_ term) ++ res) [] blocks in

  sortAndGroup edges

  where incomingEdges name_ term =
          case term of
            Do (CondBr _ targetName1 targetName2 _) -> [(targetName1, name_), (targetName2, name_)]
            Do (Br targetName _) -> [(targetName, name_)]
            _ -> []
        sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]


removeUnneededRegisters :: [BasicBlock] -> [BasicBlock]
removeUnneededRegisters blocks =
  let usedRegisters = Set.fromList $ concatMap
        (\inst -> [name_ | LocalReference _ name_ <- childrenBi inst]) $ toInstructionList blocks
      usedTerminatorRegs = Set.fromList $ concatMap
        (\inst -> [ name_ | LocalReference _ name_ <- childrenBi inst ]) $ toTermsList blocks
      neededRegs = Set.union usedRegisters usedTerminatorRegs in
  map (\(BasicBlock name insts term) -> BasicBlock name (mapMaybe (\a ->
    case a of
      name_ := inst ->
        if Set.member name_ neededRegs then Just a
        else case inst of
               Call {} -> Just a
               _ -> Nothing
      Do _ -> Just a) insts) term) blocks


toInstructionList :: [BasicBlock] -> [Named Instruction]
toInstructionList = concatMap toInstructions


toTermsList :: [BasicBlock] -> [Named Terminator]
toTermsList = map (\(BasicBlock _ _ t) -> t)


toInstructions :: BasicBlock -> [Named Instruction]
toInstructions (BasicBlock _ ins _) = ins
