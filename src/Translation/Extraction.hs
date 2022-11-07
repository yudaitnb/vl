module Translation.Extraction where

import qualified Data.Map as M
import Data.Map (Map, (!))
import Syntax.LambdaVL
import Syntax.Name
import Syntax.Label (Label)
import Syntax.Type (Type)
import Syntax.SrcLoc (SrcSpanInfo(..))
import DependencyGraph (VLMod(..))
import Inference.TypeInference (TypedExp)
import Data.Maybe (fromMaybe)
import Util

extract :: Map String (String, VLMod, Type, Label) -> Map String (Exp SrcSpanInfo) -> Exp SrcSpanInfo -> Exp SrcSpanInfo
extract exVarLabels vldecls = extractExp
  where
    extractExp :: Exp SrcSpanInfo -> Exp SrcSpanInfo
    extractExp e = case e of
      Var l qn ->
        case M.lookup (getName qn) exVarLabels of
          Nothing  -> e
          Just (orig, vlmod, _, label) ->
            -- error $ show orig
            let subst = fromMaybe
                        (error $ putDocString $ line <> ppP orig <+> ppP "is not included in" <+> ppP (show vldecls)) $
                        M.lookup orig vldecls
            in extractExp subst
      Lit l lit -> e
      App l e1 e2 -> App l (extractExp e1) (extractExp e2)
      Lambda l p e -> Lambda l p (extractExp e)
      If l e1 e2 e3 -> If l (extractExp e1) (extractExp e2) (extractExp e3)
      Pr l e -> Pr l (extractExp e)
      VRes l label e -> VRes l label (extractExp e)
      VExt l e -> VExt l (extractExp e)