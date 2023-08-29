module Syntax.Common.SrcLoc (
  Language.Haskell.Exts.SrcLoc.SrcLoc(..),
  Language.Haskell.Exts.SrcLoc.SrcSpanInfo(..),
  Language.Haskell.Exts.SrcLoc.SrcInfo(..),
  Language.Haskell.Exts.SrcLoc.noSrcSpan,
) where

import Language.Haskell.Exts.SrcLoc
import Util

------------------------------------

instance PrettyAST SrcSpanInfo where
  ppP _ = ppP ""

instance PrettyAST SrcSpan where
  ppP _ = ppP ""