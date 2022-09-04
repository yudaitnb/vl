module Syntax.SrcLoc (
  Language.Haskell.Exts.SrcLoc.SrcLoc(..),
  Language.Haskell.Exts.SrcLoc.SrcSpanInfo(..),
  Language.Haskell.Exts.SrcLoc.SrcInfo(..),
) where

import Language.Haskell.Exts.SrcLoc
import Util

------------------------------------

instance PrettyAST SrcSpanInfo where
  ppE (SrcSpanInfo srcInfoSpan srcInfoPoints@[]) =
        ppE "(SrcSpanInfo"
    <+> ppE srcInfoSpan
    <+> pplist ppE srcInfoPoints <> ppE ")"
  ppE (SrcSpanInfo srcInfoSpan srcInfoPoints) =
        nest 2 $ ppE "(SrcSpanInfo" <> line
    <+> ppE srcInfoSpan <> line
    <+> pplist ppE srcInfoPoints <> ppE ")"
  ppP _ = ppP ""

instance PrettyAST SrcSpan where
  ppE (SrcSpan srcSpanFilename srcSpanStartLine srcSpanStartColumn srcSpanEndLiine srcSpanEndColumn) =
       ppE "(SrcSpan "
    <> ppE srcSpanFilename
    <> ppE "@(" <> ppE srcSpanStartLine <> comma <> ppE srcSpanStartColumn <> ppE ")-"
    <> ppE "(" <> ppE srcSpanEndLiine <> comma <> ppE srcSpanEndColumn <> ppE ")"
    <> ppE ")"
  ppP _ = ppP ""