{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Solver.Engine.SolverGraph where

import Prelude

-- import Data.Map
import GraphTrans ( labelDeps, sortedVarDeps )
import Algebra.Graph.ToGraph (topSort)
import Syntax.Type
import Syntax.Common hiding (Name(..))
import Data.Map (Map, empty, fromList, toList, delete, insert, insertWith, size, mapWithKey)
import Util
import Control.Monad.State
import Syntax.Substitution (typeSubstitution, makeSubst, constraintsSubstitution)

-- やること
-- 1. トップレベル宣言が不要になるように書き直す
-- 2. ユニットテストを通るように書き直す(solveからEither値を返す)

-- exmods = [("A", [v100,v101]), ("B", [v100,v101])]
-- idxmods = [("A", 0), ("B", 1)]
-- idxvers = [ ("A", [(v100, 0), (v101, 1)])
--           , ("B", [(v100, 0), (v101, 1)])]
-- idxvers' = [ (0, [(0, v100), (1, v101)])
--            , (1, [(0, v100), (1, v101)])]
-- l = length idxmods

type ErrMsg = String
type SRError = (ErrMsg, [String])
type SRSuccess = Map VarName Label
type SolverResult = Either SRError SRSuccess

csToLabels :: [Constraints] -> Map String Label
csToLabels cs = case cs of
  [] -> empty
  (CSubset (TyVar n1) (TyLabels l2)) : cs -> insert (getName n1) l2 $ csToLabels cs
  _ -> error $ putDocString $ ppP cs

solve :: Map String [Version] -> Constraints -> IO SolverResult
solve em cs = do
  return $ Right (csToLabels (solveAnd cs))

-- 全てがLabelDpendencyならば、そのCsLstは解けている
solved :: [Constraints] -> Bool
solved = all (\x -> case x of
                CSubset (TyVar _) (TyLabels _) -> True
                _                              -> False)

solveAnd :: Constraints -> [Constraints]
solveAnd cs = solveAnd' $ mkOrdLstOfAndCs cs
  where
    solveAnd' :: [Constraints] -> [Constraints]
    solveAnd' cs = case cs of
      [] -> []
      c : cs' ->
        if solved cs then cs
        else case c of
          CSubset _ _ -> c : solveAnd' (map (composeCs c) cs)
          _           -> error ""

    composeCs :: Constraints -> Constraints -> Constraints
    composeCs c1 c2 = case c1 of
      CSubset (TyVar n11) (TyLabels l12) ->
        case c2 of
          CSubset (TyVar n21) (TyLabels l22) ->
            if n11 == n21
              then CSubset (TyVar n11) (TyLabels $ composeLabels l12 l22)
              else c2
          CSubset (TyVar n21) (TyVar n22) ->
            if n11 == n21
              then error "" -- ソート済みなのでこのケースはないはず
              else constraintsSubstitution (makeSubst (getName n11) (TyLabels l12)) c2
          _ -> error ""
      _ -> error ""

mkOrdLstOfAndCs :: Constraints -> [Constraints]
mkOrdLstOfAndCs cs = labelDeps cs ++ sortedVarDeps cs

instance PrettyAST [[Constraints]] where
  ppP cs = brackets $ concatWith (surround $ comma <> line) $ map ppP cs

instance PrettyAST [Constraints] where
  ppP cs = brackets $ concatWith (surround $ comma <> space) $ map ppP cs