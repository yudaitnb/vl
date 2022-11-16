module Syntax.Common.Keys (
  ModName(..),
  VarName(..),
  VarKey(..),
  getVN, getMN,
) where

-- import Syntax.Common.SrcLoc ( SrcSpanInfo )
-- import Syntax.Common.Name ( QName )

data VarKey =
    QVar ModName VarName
  | UQVar VarName
  deriving (Eq, Ord, Show)

getVN :: VarKey -> VarName
getVN vk = case vk of
  UQVar vn  -> vn
  QVar mn vn -> vn

getMN :: VarKey -> Maybe VarName
getMN vk = case vk of
  UQVar vn  -> Nothing
  QVar mn vn -> Just mn

type ModName = String
type VarName = String
-- type VarName = QName SrcSpanInfo
