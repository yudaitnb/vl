{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Solver where

import Prelude

-- import Data.Map
import Graph
import Syntax.Type
import Syntax.Common hiding (Name(..))

import Util

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

-- type ErrMsg = String
-- type SRError = (ErrMsg, [String])
-- type SRSuccess = Map VarName Label
-- type SolverResult = Either SRError SRSuccess


-- cvtCs :: Constraints -> [Constraints] -> [[Constraints]]
-- cvtCs cs st = case cs of
--   CSubset _ _ -> 
--   CAnd c1 c2  -> c1 
--   COr  c1 c2  -> (cvtCs c1 []) (cvtCs c2 [])
--   CTop        -> 

-- solve :: Constraints -> IO SolverResult
-- solve cs = return ("a", ["a"])

-- labelDep1, ..., labelDepN, VarDep1 (upper), ... , VarDepN (lower)

-- 全てがLabelDpendencyかつ・・・ならば、そのCsLstは解けている
solved :: [Constraints] -> Bool
solved = all (\x -> case x of
                CSubset (TyVar _) (TyLabels _) -> True
                _                              -> False)

-- mkOrdLstOfCs :: Constraints -> []

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
      sorted  = tsort g
  in labelDeps ++ sortCssBy sorted varDeps
  where
    spanEdges :: VarKey -> [Constraints] -> ([Constraints], [Constraints])
    spanEdges k = span (\(CSubset c1 _) -> k `elem` freeVars c1)

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

shrinkCs :: Constraints -> Constraints
shrinkCs cs =
  let los = leftOccur cs in
  shrinkCs' cs los
  where
    shrinkCs' :: Constraints -> [VarKey] -> Constraints
    shrinkCs' cs los = case cs of 
      CTop -> CTop
      CSubset c1 c2 -> if UQVar (getName c2) `elem` los then cs else CTop
      CAnd c1 CTop  -> c1
      CAnd CTop c2  -> c2
      CAnd c1 c2    -> CAnd (shrinkCs' c1 los) (shrinkCs' c2 los)
      COr CTop _    -> CTop
      COr _ CTop    -> CTop
      COr c1 c2     -> COr  (shrinkCs' c1 los) (shrinkCs' c2 los)

leftOccur :: Constraints -> [VarKey]
leftOccur cs = case cs of
  CTop          -> []
  CSubset c1 c2 -> [UQVar (getName c1)]
  CAnd c1 c2    -> leftOccur c1 ++ leftOccur c2
  COr c1 c2     -> leftOccur c1 ++ leftOccur c2

leftOccurCss :: [Constraints] -> [VarKey]
leftOccurCss = concatMap leftOccur
-- 

-- cSubToTuple :: Constraints -> [(VarKey, VarKey)]
-- cSubToTuple cs = case cs of
--   CSubset (TyVar n1) (TyVar n2) -> [(UQVar n1, UQVar n2)]
--   CSubset (TyVar n1) (TyLabels ls) -> []
--   _          -> error ""

-- -- tupleToCSub :: (Name, Name) -> Constraints
-- -- tupleToCSub (n1, n2) = CSubset (TyVar n1) (TyVar n2)

-- solveAnd :: Constraints -> Constraints
-- solveAnd cs = let
--   g = Graph ((freeVars cs), cSubToTuple (cvtAnd cs))
--   nodes = tsort g
--   in nodes

instance PrettyAST [[Constraints]] where
  ppE cs = brackets $ concatWith (surround $ comma <> line) $ map ppE cs
  ppP cs = brackets $ concatWith (surround $ comma <> line) $ map ppP cs

instance PrettyAST [Constraints] where
  ppE cs = brackets $ concatWith (surround $ comma <> space) $ map ppE cs
  ppP cs = brackets $ concatWith (surround $ comma <> space) $ map ppP cs