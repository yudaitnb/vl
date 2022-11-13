module Syntax.Common.Label where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.List as L
import Data.Monoid

import Syntax.Common.Version
import Syntax.Common.Keys
import Util
import Data.Bifunctor (second)

-- Label: その値の取りうる各モジュールのバージョンの候補を集めたもの
-- e.g.)
-- fromList [ ("A", [v100,v101]) Aへの依存性はv100,v101のいずれかであり、他のバージョンではない
--          , ("B", [v100])      Bへの依存性はv100であり、他のバージョンではない
--          , ("C", []) ]        Cについては無制約
type Label = Map String [Version]


-- FixedLabel: 全てのモジュールについてバージョンが一つに定まっているラベル
-- e.g.)
-- fromList [ ("A", v101) Aへの依存性はv100,v101のいずれかであり、他のバージョンではない
--          , ("B", v100)      Bへの依存性はv100であり、他のバージョンではない
--          , ("C", v100) ]        Cについては無制約
type FixedLabel = Map String Version

isValidFixedLabel :: Label -> FixedLabel -> Bool
isValidFixedLabel label fl = M.difference label fl == M.empty && 
                             M.isSubmapOfBy L.elem fl label

emptyLabels :: Label
emptyLabels = M.empty

pickVersion :: ([Version] -> Version) -> Label -> FixedLabel
pickVersion = M.map

pickLatestVersion :: Label -> FixedLabel
pickLatestVersion = pickVersion maximum -- Version 1 0 0 <= Version 1 0 1

mkFixedLabel :: [(String, [(String, Version)])] -> Map VarName FixedLabel
mkFixedLabel m = M.fromList $ L.map (\(vn,l) -> (vn, M.fromList l)) m

------------------------

instance PrettyAST Label where
  ppE labels = braces $ concatWith (surround $ comma <> space) $ L.map (\(mn,vers) -> ppE mn <> colon <> ppE vers) $ M.toList labels
  ppP labels = braces $ concatWith (surround $ comma <> space) $ L.map (\(mn,vers) -> ppP mn <> colon <> ppP vers) $ M.toList labels

instance PrettyAST FixedLabel where
  ppE fl = braces $ concatWith (surround $ comma <> space) $ L.map (\(mn,vers) -> ppE mn <> colon <> ppE vers) $ M.toList fl
  ppP fl = braces $ concatWith (surround $ comma <> space) $ L.map (\(mn,vers) -> ppP mn <> colon <> ppP vers) $ M.toList fl