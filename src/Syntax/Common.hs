module Syntax.Common (
  module Syntax.Common.Label,
  module Syntax.Common.SrcLoc,
  module Syntax.Common.Version,
  module Syntax.Common.Name,
  module Syntax.Common.Literal,
  module Syntax.Common.Keys,
  module Syntax.Common.HasVar,
  VLMod(..),
  ParsedAST,
  reservedOps,
  mkVKFromQN, mkQNFromVK
) where

import Data.Map
import Language.Haskell.Exts.Syntax as Absyn
import Syntax.Common.Version
import Syntax.Common.SrcLoc
import Syntax.Common.Label
import Syntax.Common.Literal
import Syntax.Common.Name
import Syntax.Common.Keys
import Syntax.Common.HasVar

import Util

type ParsedAST = Map VLMod (Absyn.Module SrcSpanInfo)

data VLMod = VLMod {
    modName :: ModName
  , version :: Version
  }
  deriving (Eq, Ord, Show)

reservedOps :: [String]
reservedOps = [ "+", "-", "*", "/"
              , "&&", "||"
              , "<" ,"<=", ">", ">=", "==", "/="]

mkVKFromQN :: QName l -> VarKey
mkVKFromQN qn = case qn of
  Qual _ mn n -> QVar (getName mn) (getName n)
  UnQual _ n  -> UQVar (getName n)

mkQNFromVK :: VarKey -> QName SrcSpanInfo
mkQNFromVK qk = case qk of
  QVar mn n -> mkQual mn n
  UQVar n  -> mkUnQual n

----------------------

instance PrettyAST VLMod where
  ppP (VLMod mn v) = ppP mn <> ppP "-" <> ppP v

instance PrettyAST ParsedAST where
  ppP m
    | Data.Map.null m = ppP "{}"
    | otherwise = concatWith (surround line) $
      mapWithKey (\vlmod mod -> ppP "===" <+> ppP vlmod <> line <> ppP (exactPrint mod [])) m

instance PrettyAST VarKey where
  ppP qk = case qk of
    QVar mn vn -> ppP mn <> dot <> ppP vn
    UQVar vn  -> ppP vn