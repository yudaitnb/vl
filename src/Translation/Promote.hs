module Translation.Promote where

import Language.LambdaVL
import Syntax.Common

promoteTopVal :: Module l -> Module l
promoteTopVal (Module l mh pragmas imp decls) = Module l mh pragmas imp (map promoteDecl decls)
  where
    promoteDecl :: Decl l -> Decl l
    promoteDecl pb@(PatBind l1 pvar exp) =
      if getName pvar == "main"
        then pb
        else PatBind l1 pvar (promoteExp exp) -- [TODO] not l1

    promoteExp :: Exp l -> Exp l
    promoteExp exp = case exp of
      App l1 (Lambda l2 p e1) e2     -> App l1 (Lambda l2 p (promoteExp e1)) e2
      -- App l1 (Lambda l2 p e1) e2  -> App l1 (Lambda l2 p (promoteExp e1)) e2
      Lambda l1 p (Case l2 exp alts) -> Lambda l1 p (Case l2 exp $ map promoteAlt alts)
      _ -> Pr (ann exp) exp
    
    promoteAlt :: Alt l -> Alt l
    promoteAlt alt = case alt of
      Alt l p e -> Alt l p $ promoteExp e