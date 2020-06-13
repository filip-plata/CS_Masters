module Main where

import Control.Monad.State
import Test.QuickCheck

import Formula
import Utils
import SatSolver
import Data.Set (Set)
import qualified Data.Set as Set


instance Arbitrary Formula where
    arbitrary = sized f where

      f 0 = oneof $ map return $ (map Var ["p", "q", "r", "s", "t"]) ++ [T, F]

      f size = frequency [
        (1, liftM Not (f (size - 1))),
        (4, do
              size' <- choose (0, size - 1)
              conn <- oneof $ map return [And, Or, Implies, Iff]
              left <- f $ size'
              right <- f $ size - size' - 1
              return $ conn left right)
        ]


-- finds all variables occurring in the formula (sorted, without duplicates)
variables :: Formula -> [VarName]
variables = rmdups . go where
  go T = []
  go F = []
  go (Var x) = [x]
  go (Not phi) = go phi
  go (And phi psi) = go phi ++ go psi
  go (Or phi psi) = go phi ++ go psi
  go (Implies phi psi) = go phi ++ go psi
  go (Iff phi psi) = go phi ++ go psi

-- truth valuations
type Valuation = VarName -> Bool

-- the evaluation function
eval :: Formula -> Valuation -> Bool
eval T _ = True
eval F _ = False
eval (Var x) rho = rho x
eval (Not phi) rho = not $ eval phi rho
eval (And phi psi) rho = (eval phi rho) && (eval psi rho)
eval (Or phi psi) rho = (eval phi rho) || (eval psi rho)
eval (Implies phi psi) rho = not (eval phi rho) || (eval psi rho)
eval (Iff phi psi) rho = eval phi rho == eval psi rho

-- updating a truth valuation
extend :: Valuation -> VarName -> Bool -> Valuation
extend rho x v y
  | x == y = v
  | otherwise = rho y

-- the list of all valuations on a given list of variables
valuations :: [VarName] -> [Valuation]
valuations [] = [undefined] -- any initial valuation would do
valuations (x : xs) = concat [[extend rho x True, extend rho x False] | rho <- valuations xs]

-- satisfiability checker based on truth tables
satisfiable :: Formula -> Bool
satisfiable phi = or [eval phi rho | rho <- valuations (variables phi)]

equi_satisfiable :: Formula -> Formula -> Bool
equi_satisfiable phi psi = satisfiable phi == satisfiable psi

-- convert a CNF in the list of clauses form to a formula
-- entirely analogous to "dnf2formula" from Lab01
cnf2formula :: [[Literal]] -> Formula
cnf2formula [] = T
cnf2formula lss = foldr1 And (map go lss) where
  go [] = F
  go ls = foldr1 Or (map go2 ls)
  go2 (Pos x) = Var x
  go2 (Neg x) = Not (Var x)

-- test for ecnf
prop_ecnf :: Formula -> Bool
prop_ecnf phi = equi_satisfiable phi (cnf2formula $ ecnf phi)

main:: IO ()
main = quickCheckWith (stdArgs {maxSize = 5}) prop_ecnf
