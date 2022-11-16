module Translation.Extraction where

import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)

import Language.LambdaVL

import Syntax.Common
import Syntax.Type ( Type ) 

import Parser (VLMod(..))
import Inference.TypeInference (TypedExp)

import Util

extract :: Map VarName (VarName, VLMod, Type, Label) -> Map VLMod (Map VarName (Exp SrcSpanInfo)) -> Exp SrcSpanInfo -> Exp SrcSpanInfo
extract exVarLabels vldecls = extractExp
  where
    extractExp :: Exp SrcSpanInfo -> Exp SrcSpanInfo
    extractExp e = case e of
      Var l qn ->
        case M.lookup (getName qn) exVarLabels of
          Nothing  -> e
          Just (orig, vlmod, _, label) ->
            let (mn, vers) = head (M.toList label)
                target = VLMod mn (head vers)
                subst = fromMaybe
                        (error $ putDocString $ line <> ppP orig <+> ppP "is not included in" <+> ppP (show $ vldecls <!> target)) $
                        M.lookup orig (vldecls <!> target)
            in extractExp subst
      Lit l lit     -> e
      App l e1 e2   -> App l (extractExp e1) (extractExp e2)
      Lambda l p e  -> Lambda l p (extractExp e)
      Tuple l elms  -> Tuple l (map extractExp elms)
      List l elms   -> List l (map extractExp elms)
      Case l e alts -> Case l e $ map (\(Alt l' p' e') -> Alt l' p' $ extractExp e') alts
      If l e1 e2 e3 -> If l (extractExp e1) (extractExp e2) (extractExp e3)
      Pr l e        -> Pr l (extractExp e)
      VRes l lbl e  -> VRes l lbl (extractExp e)
      VExt l e      -> VExt l (extractExp e)