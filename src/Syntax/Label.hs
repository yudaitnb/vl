module Syntax.Label where

import Data.Map
import Data.List
import Data.Set
import Data.Monoid

import Syntax.Version
import Util

-- e.g.)
-- fromList [ ("A", [v100,v101])
--          , ("B", [v100])
--          , ("C", []) ]
type Labels = Map String [Version]

emptyLabels :: Labels
emptyLabels = Data.Map.empty

------------------------

-- instance PrettyAST Label where
--   ppE (Label m) = nest 2 $ ppE "(Label" <+> pplist ppE (Data.Map.toList m) <> ppE ")"
--   ppP (Label m) = 
--     parens $ concatWith (surround comma) $
--       Data.List.map
--         (\(k,v) -> ppP k <> ppP ":" <> ppP v)
--         (Data.Map.toList m)

instance PrettyAST Labels where
  -- ppE = Data.Map.foldrWithKey (\mn vs acc -> ppE mn <> colon <> ppE vs <> comma <+> acc) emptyDoc
  ppE labels = braces $ concatWith (surround $ comma <> space) $ Data.List.map (\(mn,vers) -> ppE mn <> colon <> ppE vers) $ Data.Map.toList labels
  ppP labels = braces $ concatWith (surround $ comma <> space) $ Data.List.map (\(mn,vers) -> ppP mn <> colon <> ppP vers) $ Data.Map.toList labels