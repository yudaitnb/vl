module GraphTrans where

import Algebra.Graph
import Algebra.Graph.ToGraph (topSort)

import Syntax.Common hiding (Name(..), QName(..))
import Syntax.Type
import qualified Data.Map as M
import qualified Data.Bifunctor

import Util
import Data.Either (fromRight)

g0 = vertex 0
g1 = vertex 1
g2 = connect g0 g1
g3 = overlay g0 g1

data VV = LVV VarKey | VV VarKey

instance HasName VV where
  getName vv = case vv of
    LVV vk -> getName vk
    VV vk  -> getName vk


instance Show VV where
  show vv = case vv of
    LVV vk -> "(" ++ getName vk ++ ")"
    VV vk -> getName vk

instance Eq VV where
  LVV vk1 == LVV vk2 = vk1 == vk2
  VV vk1 == VV vk2 = vk1 == vk2
  _ == _ = False

instance Ord VV where
  LVV vk1 <= LVV vk2 = vk1 <= vk2
  VV vk1  <= VV vk2  = vk1 <= vk2
  VV _    <= LVV _   = True
  LVV _   <= VV _    = False

edgeToCs :: (VV, VV) -> Constraints
edgeToCs (vv1, vv2) = CSubset (TyVar . Ident $ getName vv1) (TyVar . Ident $ getName vv2)

getVarKey :: VV -> VarKey
getVarKey vv = case vv of
  LVV vk -> vk
  VV vk -> vk

labelDeps :: Constraints -> [Constraints]
labelDeps cs = case cs of
  CTop          -> []
  CSubset c1@(TyVar _) c2@(TyLabels l2) -> [cs]
  CSubset c1@(TyVar _) c2@(TyVar _) -> []
  CAnd c1 c2    -> labelDeps c1 ++ labelDeps c2 
  COr c1 c2     -> labelDeps c1 ++ labelDeps c2
  _ -> error ""

labelDepsKeys :: Constraints -> [VarKey]
labelDepsKeys cs = case cs of
  CTop          -> []
  CSubset c1@(TyVar _) c2@(TyLabels l2) -> [UQVar (getName c1)]
  CSubset c1@(TyVar _) c2@(TyVar _) -> []
  CAnd c1 c2    -> labelDepsKeys c1 ++ labelDepsKeys c2 
  COr c1 c2     -> labelDepsKeys c1 ++ labelDepsKeys c2
  _ -> error ""

mkCsGraph :: Constraints -> Graph VV
mkCsGraph cs = mkCsGraph' (labelDepsKeys cs) cs
  where
    mkCsGraph' :: [VarKey] -> Constraints -> Graph VV
    mkCsGraph' lbl cs = case cs of
      CTop          -> empty
      CSubset c1@(TyVar _) c2@(TyLabels l2) -> vertex . LVV . UQVar $ getName c1
      CSubset c1@(TyVar _) c2@(TyVar _) ->
        let [vv1,vv2] = map (\c ->
                              let vk = UQVar (getName c) in
                              if vk `elem` lbl then LVV vk else VV vk)
                            [c1,c2]
        in edge vv1 vv2
      CAnd c1 c2    -> overlays $ map (mkCsGraph' lbl) [c1,c2] 
      COr c1 c2     -> overlays $ map (mkCsGraph' lbl) [c1,c2]
      _ -> error ""


sortedVarDeps :: Constraints -> [Constraints]
sortedVarDeps cs = map edgeToCs $ reverse . edgeList $ mkCsGraph cs

-------------

sort_ cs = case topSort $ mkCsGraph cs of
  Left _ -> error ""
  Right x -> reverse x

cs1 = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100 ]
cs2 = cand 
      [ mkGtLabels "a67" b100
      , mkGtVar "a501" "a68"
      , mkGtLabels "a68" b100
      , mkGtVar "a103" "a500"
      , mkGtVar "a103" "a65"
      , mkGtLabels "a65" b100
      , mkGtVar "a97" "a500"
      , mkGtVar "a97" "a66"
      , mkGtLabels "a66" b101
      , mkGtVar "a500" "a501"
      , mkGtVar "a501" "a65"
      , mkGtVar "a1" "a97"
      , mkGtVar "a1" "a103"
      ]
cs2' = sort_ cs2

cs3 = cand [ mkGtVar "a0" "a1", mkGtVar "a1" "a2", mkGtLabels "a2" a100 ]
cs3' = sort_ cs3

cs4 = cand [ mkGtVar "a0" "a1", mkGtLabels "a1" a100 ]
cs4' = sort_ cs4


mkGtLabels :: String -> Label -> Constraints
mkGtLabels vn labels = CSubset (mkVar vn) (TyLabels labels)

mkGtVar :: String -> String -> Constraints
mkGtVar vn1 vn2 = CSubset (mkVar vn1) (mkVar vn2)

a100, a101, b100, b101 :: Label
a100 = mkLabel [("A", [v100])]
a101 = mkLabel [("A", [v101])]
b100 = mkLabel [("B", [v100])]
b101 = mkLabel [("B", [v101])]

cand :: [Constraints] -> Constraints
cand = foldr CAnd CTop

v100, v101, tbd :: Version
v100 = Version 1 0 0
v101 = Version 1 0 1
tbd  = TBD

mkLabel :: [(ModName, [Version])] -> Label
mkLabel  = M.fromList

mkLabels :: [(VarName, [(ModName, [Version])])] -> M.Map VarName Label
mkLabels lst = M.fromList $ map (Data.Bifunctor.second mkLabel) lst

mkVar :: VarName -> Type
mkVar s = TyVar $ Ident s