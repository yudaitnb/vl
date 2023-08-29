{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Solver where

import Prelude

-- import Data.Map
import Graph
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
  _ -> error ""

solve :: Map String [Version] -> Constraints -> IO SolverResult
solve em cs = do
  return $ Right (csToLabels (solveAnd cs))

-- 全てがLabelDpendencyならば、そのCsLstは解けている
solved :: [Constraints] -> Bool
solved = all (\x -> case x of
                CSubset (TyVar _) (TyLabels _) -> True
                _                              -> False)

solveAnd :: Constraints -> [Constraints]
solveAnd cs =
  let
    ord = mkOrdLstOfAndCs cs
  in
    solveAnd' ord
  where
    solveAnd' :: [Constraints] -> [Constraints]
    solveAnd' cs = case cs of
      [] -> []
      c : cs' ->
        if solved cs then cs
        else case c of
          CSubset _ _ -> solveAnd' (map (composeCs c) cs)
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
mkOrdLstOfAndCs cs =
  let flattened = cvtAnd cs
      (varDeps, labelDeps) = spanCsToDeps cs
      nodes = freeVars cs
      edges = foldr
        (\dep acc -> case dep of
          CSubset tv1@(TyVar _) tv2@(TyVar _) ->
            (UQVar (getName tv1), UQVar (getName tv2)) : acc)
        []
        varDeps
      g = Graph (nodes, edges)
      sorted  = reverse $ tsort g
  in sortCssBy sorted flattened
  where
    spanEdges :: VarKey -> [Constraints] -> ([Constraints], [Constraints])
    spanEdges k = span (\(CSubset c1 c2) -> all (\c -> k `elem` freeVars c) [c1,c2])

    sortCssBy :: [VarKey] -> [Constraints] -> [Constraints]
    sortCssBy ks css =  case ks of
      [] -> css
      k : xs ->
        let (accumrated, rst) = spanEdges k css
        in accumrated ++ sortCssBy xs rst

    spanCsToDeps :: Constraints -> ([Constraints], [Constraints])
    spanCsToDeps cs =
      let flattened = cvtAnd cs in
      span
        (\dep -> case dep of
          CSubset (TyVar _) (TyVar _) -> True
          _ -> False)
        flattened

    andOnly :: Constraints -> Bool
    andOnly cs = case cs of
      CTop        -> True
      CSubset _ _ -> True
      CAnd c1 c2  -> andOnly c1 && andOnly c2
      COr  c1 c2  -> False

    cvtAnd :: Constraints -> [Constraints]
    cvtAnd cs = case cs of
      CTop        -> []
      CSubset _ _ -> [cs]
      CAnd c1 c2  -> cvtAnd c1 ++ cvtAnd c2
      COr  c1 c2  -> [] -- errorにすべき

instance PrettyAST [[Constraints]] where
  ppE cs = brackets $ concatWith (surround $ comma <> line) $ map ppE cs
  ppP cs = brackets $ concatWith (surround $ comma <> line) $ map ppP cs

instance PrettyAST [Constraints] where
  ppE cs = brackets $ concatWith (surround $ comma <> space) $ map ppE cs
  ppP cs = brackets $ concatWith (surround $ comma <> space) $ map ppP cs