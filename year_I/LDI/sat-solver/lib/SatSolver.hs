module SatSolver where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List (sortBy)
import Data.Function (on)

import Utils
import Formula

data Literal = Pos VarName | Neg VarName deriving (Eq, Show, Ord)

opposite :: Literal -> Literal
opposite (Pos p) = Neg p
opposite (Neg p) = Pos p


simplify :: Formula -> Formula

simplify T = T
simplify F = F
simplify (Var p) = Var p

simplify (Not T) = F
simplify (Not F) = T
simplify (Not (Not phi)) = simplify phi
simplify (Not phi) = Not (simplify phi)

simplify (And T phi) = simplify phi
simplify (And phi T) = simplify phi
simplify (And F _) = F
simplify (And _ F) = F
simplify (And phi psi) = And (simplify phi) (simplify psi)

simplify (Or T _) = T
simplify (Or _ T) = T
simplify (Or F phi) = simplify phi
simplify (Or phi F) = simplify phi
simplify (Or phi psi) = Or (simplify phi) (simplify psi)

simplify (Implies T phi) = simplify phi
simplify (Implies _ T) = T
simplify (Implies F _) = T
simplify (Implies phi F) = simplify (Not phi)
simplify (Implies phi psi) = Implies (simplify phi) (simplify psi)

simplify (Iff T phi) = simplify phi
simplify (Iff phi T) = simplify phi
simplify (Iff F phi) = simplify (Not phi)
simplify (Iff phi F) = simplify (Not phi)
simplify (Iff phi psi) = Iff (simplify phi) (simplify psi)

-- keep simplifying until no more simplifications are possible
deepSimplify :: Formula -> Formula
deepSimplify phi = go where
  psi = simplify phi
  go | phi == psi = phi
     | otherwise = deepSimplify psi


nnf :: Formula -> Formula
nnf T = T
nnf F = F
nnf (Not T) = F
nnf (Not F) = T
nnf (Var p) = Var p
nnf (Not (Var p)) = Not $ Var p
nnf (And phi psi) = And (nnf phi) (nnf psi)
nnf (Or phi psi) = Or (nnf phi) (nnf psi)
nnf (Implies phi psi) = nnf (Or (Not phi) psi)
nnf (Iff phi psi) = nnf (And (Implies phi psi) (Implies psi phi))
nnf (Not (Not phi)) = nnf phi
nnf (Not (And phi psi)) = nnf (Or (Not phi) (Not psi))
nnf (Not (Or phi psi)) = nnf (And (Not phi) (Not psi))
nnf (Not (Implies phi psi)) = nnf (And phi (Not psi))
nnf (Not (Iff phi psi)) = nnf (Or (And phi (Not psi)) (And (Not phi) psi))


cnf :: Formula -> [[Literal]]
cnf phi = go $ nnf phi where
  go T = []
  go F = [[]]
  go (Var x) = [[Pos x]]
  go (Not (Var x)) = [[Neg x]]
  go (And phi psi) = go phi ++ go psi
  go (Or phi psi) = distribute (go phi) (go psi)


ecnf :: Formula -> [[Literal]]
ecnf phi =
  let (top, cons, _) = go (nnf phi, [], 0)
   in cnf top ++ cons
  where
    go (And phi psi, l, n) = binaryOpEquiv And l n phi psi
    go (Or phi psi, l, n) = binaryOpEquiv Or l n phi psi
    go (f, l, n) = (f, l, n)
    binaryOpEquiv op l n phi psi =
      let (phi1, l1, n1) = go (phi, l, n)
          (psi1, l2, n2) = go (psi, l1, n1)
          varName = "x_phi__" ++ show n2
          equivFormula = Iff (Var varName) $ op phi1 psi1
       in (Var varName, cnf equivFormula ++ l2, n2 + 1)


type Model = Set Literal
type CNF = Set (Set Literal)

varName :: Literal -> VarName
varName (Pos p) = p
varName (Neg p) = p

positiveLiterals :: Set Literal -> [VarName]
positiveLiterals ls = rmdups [p | Pos p <- Set.toList ls]

negativeLiterals :: Set Literal -> [VarName]
negativeLiterals ls = rmdups [p | Neg p <- Set.toList ls]

literals :: Set Literal -> [VarName]
literals ls = rmdups $ positiveLiterals ls ++ negativeLiterals ls

pureLiteralRule :: CNF -> CNF
pureLiteralRule cnfs =
  let posi = Set.fromList (concatMap positiveLiterals cnfs)
      negi = Set.fromList (concatMap negativeLiterals cnfs)
      common = Set.intersection posi negi
      pure = Set.difference (Set.union posi negi) common in
  Set.filter (\ls -> Set.null $ Set.intersection (Set.fromList (literals ls)) pure) cnfs


removeTautologies :: CNF -> CNF
removeTautologies cnfs = Set.fromList $ remove isTautology $ Set.toList cnfs
  where
    isTautology lits = or $ Set.toList $ Set.map (\lit -> Set.member (opposite lit) lits) lits


unitPropagation :: CNF -> CNF
unitPropagation cnfs =
  let singleLits = single_literals cnfs
      singleLitsSet = remove_opposite_units $ Set.fromList singleLits
      cnfsUnitRemoved = Set.filter (null . Set.intersection singleLitsSet) cnfs
   in Set.map (Set.fromList . remove (\l -> opposite l `Set.member` singleLitsSet) . Set.toList) cnfsUnitRemoved
  where
    single_literals lss =
      mapMaybe
        (\ls ->
           if Set.size ls /= 1
             then Nothing
             else Just $ head $ Set.toList ls)
      (Set.toList lss)
    remove_opposite_units lits = Set.fold
      (\lit units ->
        if Set.member (opposite lit) lits 
          then Set.insert (Pos $ varName lit) units
          else Set.insert lit units) Set.empty lits


chooseLiteral :: CNF -> Maybe VarName
chooseLiteral cnfs = do
  smallestSet <- safeHead $ sortBy (compare `on` Set.size) $ Set.toList cnfs
  firstLit <- safeHead $ Set.toList smallestSet
  return $ varName firstLit


satSolver :: Formula -> Bool
satSolver phi =
  let cnfForm = Set.fromList $ map Set.fromList (ecnf $ deepSimplify phi)
   in go cnfForm
  where
    go cnfs =
      Set.null cnfs ||
      (not (Set.member Set.empty cnfs) &&
       (let cnfs1 = removeTautologies cnfs
            cnfs2 = unitPropagation cnfs1
         in if cnfs2 /= cnfs1
              then go cnfs2
              else let cnfs3 = pureLiteralRule cnfs2
                    in if cnfs3 /= cnfs2
                         then go cnfs3
                         else case chooseLiteral cnfs1 of
                                Just l ->
                                  go (Set.union cnfs1 (Set.singleton $ Set.singleton $ Pos l)) ||
                                  go (Set.union cnfs1 (Set.singleton $ Set.singleton $ Neg l))
                                Nothing -> go cnfs1))
