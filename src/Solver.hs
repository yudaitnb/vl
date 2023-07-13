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


data Env' = Env'
  { keys :: [VarKey]
  , flag :: Bool
  , loop :: Int
  , orig :: Constraints
  }
  deriving (Show)
type Env a = State Env' a

setFlag :: Env ()
setFlag = modify $ \env -> env { flag = True }

resetFlag :: Env ()
resetFlag = modify $ \env -> env { flag = False }

addKey :: VarKey -> Env ()
addKey k = modify $ \env -> env { keys = k : keys env }

addKeys :: [VarKey] -> Env ()
addKeys ks = modify $ \env -> env { keys = ks ++ keys env }

addLoop :: Env ()
addLoop = modify $ \env -> env { loop = 1 + loop env }

minimizeCs :: Constraints -> Constraints
minimizeCs cs =
  let initState = Env' [] False 0 cs
      env = execState (accVars cs) initState
      ks = keys env
      l = loop env
  in
    shrinkCs cs ks
    -- ks
  where
    shrinkCs :: Constraints -> [VarKey] -> Constraints
    shrinkCs cs keys = case cs of
      CTop -> CTop
      CSubset (TyVar _) (TyLabels _) -> cs
      CSubset (TyVar _) (TyVar n2) -> if UQVar (getName n2) `elem` keys then cs else CTop
      CAnd c1 CTop  -> c1
      CAnd CTop c2  -> c2
      CAnd c1 c2    -> let c1' = shrinkCs c1 keys
                           c2' = shrinkCs c2 keys in
                       case (c1', c2') of
                        (CTop, _) -> c2'
                        (_, CTop) -> c1'
                        _         -> CAnd c1' c2'
      COr CTop _    -> CTop
      COr _ CTop    -> CTop
      COr c1 c2     -> let c1' = shrinkCs c1 keys
                           c2' = shrinkCs c2 keys in
                       case (c1', c2') of
                        (CTop, _) -> CTop
                        (_, CTop) -> CTop
                        _         -> COr c1' c2'

    accVars :: Constraints -> Env ()
    accVars cs = do
      resetFlag
      ks <- gets keys
      if null ks
        then accVarsLabelDep cs
        else accVarsDepLabelDep cs
      addLoop
      gets flag >>= \f -> when f $ accVars cs

    -- Subsequent loop for variable dependencies
    accVarsDepLabelDep :: Constraints -> Env ()
    accVarsDepLabelDep cs = case cs of
      CTop -> return ()
      CSubset c1 (TyLabels _) -> return ()
      CSubset (TyVar n1) (TyVar n2) -> do
        let k1 = UQVar (getName n1)
            k2 = UQVar (getName n2)
        ks <- gets keys
        when (k1 `notElem` ks && k2 `elem` ks) $ setFlag >> addKey k1
        when (k1 `elem` ks && k2 `notElem` ks) $ setFlag >> addKey k2
      CAnd c1 c2 -> forM_ [c1,c2] accVarsDepLabelDep
      COr c1 c2  -> forM_ [c1,c2] accVarsDepLabelDep

    -- Initial loop for label dependencies
    accVarsLabelDep :: Constraints -> Env ()
    accVarsLabelDep cs = case cs of
      CTop -> return ()
      CSubset c1 (TyLabels _) -> setFlag >> addKey (UQVar (getName c1))
      CSubset c1 c2 -> return ()
      CAnd c1 c2    -> forM_ [c1,c2] accVarsLabelDep
      COr c1 c2     -> forM_ [c1,c2] accVarsLabelDep

instance PrettyAST [[Constraints]] where
  ppE cs = brackets $ concatWith (surround $ comma <> line) $ map ppE cs
  ppP cs = brackets $ concatWith (surround $ comma <> line) $ map ppP cs

instance PrettyAST [Constraints] where
  ppE cs = brackets $ concatWith (surround $ comma <> space) $ map ppE cs
  ppP cs = brackets $ concatWith (surround $ comma <> space) $ map ppP cs