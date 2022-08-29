module Syntax.SrcLoc (
  Language.Haskell.Exts.SrcLoc.SrcLoc(..),
  Language.Haskell.Exts.SrcLoc.SrcSpanInfo(..)
) where

import Language.Haskell.Exts.SrcLoc
import Prettyprinter

instance Pretty SrcSpanInfo where
  pretty (SrcSpanInfo srcInfoSpan srcInfoPoints@[]) =
        pretty "(SrcSpanInfo"
    <+> pretty srcInfoSpan
    <+> pretty srcInfoPoints <> pretty ")"
  pretty (SrcSpanInfo srcInfoSpan srcInfoPoints) =
        nest 2 $ pretty "(SrcSpanInfo" <> line
    <+> pretty srcInfoSpan <> line
    <+> pretty srcInfoPoints <> pretty ")"

instance Pretty SrcSpan where
  pretty (SrcSpan srcSpanFilename srcSpanStartLine srcSpanStartColumn srcSpanEndLiine srcSpanEndColumn) =
       pretty "(SrcSpan "
    <> pretty srcSpanFilename
    <> pretty "@(" <> pretty srcSpanStartLine <> pretty ":" <> pretty srcSpanStartColumn <> pretty ")-"
    <> pretty "(" <> pretty srcSpanEndLiine <> pretty ":" <> pretty srcSpanEndColumn <> pretty ")"
    <> pretty ")"