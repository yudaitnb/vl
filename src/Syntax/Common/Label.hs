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
type Label = Map ModName [Version]

emptyLabels :: Label
emptyLabels = M.empty

------------------------

instance PrettyAST Label where
  ppE labels = braces $ concatWith (surround $ comma <> space) $ L.map (\(mn,vers) -> ppE mn <> colon <> ppE vers) $ M.toList labels
  ppP labels = braces $ concatWith (surround $ comma <> space) $ L.map (\(mn,vers) -> ppP mn <> colon <> ppP vers) $ M.toList labels
