{-# LANGUAGE InstanceSigs #-}
module Syntax.Version (
  Version(..), vnToV,
) where

import Util
import Syntax.Absyn

data Version =
    Root
  | TBD
  | Version
    { major :: Int
    , minor :: Int
    , patch :: Int
    }
  deriving (Eq, Show)

instance Ord Version where
  (<=) :: Version -> Version -> Bool
  Root <= Root       = True
  Root <= Version {} = False
  Version {}       <= Root             = False
  Version x1 y1 z1 <= Version x2 y2 z2 = x1 <= x2 && y2 <= y2 && z1 <= z2
  TBD <= Root       = False
  TBD <= Version {} = False
  _ <= TBD = True

   

vnToV :: VersionNumber l -> Version
vnToV (VersionNumber _ major minor patch) = Version major minor patch 

instance PrettyAST Version where
  ppE v = case v of
    Root -> ppE "Root"
    TBD -> ppE "TBD"
    Version major minor patch ->
          nest 2 $ parens $ ppE "Version" <> line
      <+> ppE major <> line
      <+> ppE minor <> line
      <+> ppE patch
  ppP v = case v of
    Root -> ppP "Root"
    TBD -> ppP "TBD"
    Version major minor patch ->
      ppP major <> ppP "." <> ppP minor <> ppP "."  <> ppP patch 