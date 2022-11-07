module Translation.Promote where

import Syntax.LambdaVL
import Syntax.Name

promoteMain :: Module l -> Module l
promoteMain (Module l mh pragmas imp decls) = Module l mh pragmas imp (map promoteDecl decls)
  where
    promoteDecl :: Decl l -> Decl l
    promoteDecl (PatBind l1 pvar exp)
      | getName pvar == "main"
        = PatBind l1 pvar (promoteExp exp) -- [TODO] not l1
    promoteDecl patBind = patBind

    promoteExp :: Exp l -> Exp l
    promoteExp exp = case exp of
      App l1 (Lambda l2 p e1) e2  -> App l1 (Lambda l2 p (promoteExp e1)) e2
      _ -> Pr (ann exp) exp

promoteTopVal :: Module l -> Module l
promoteTopVal (Module l mh pragmas imp decls) = Module l mh pragmas imp (map promoteDecl decls)
  where
    promoteDecl :: Decl l -> Decl l
    promoteDecl (PatBind l1 pvar exp) 
        = PatBind l1 pvar (promoteExp exp) -- [TODO] not l1

    promoteExp :: Exp l -> Exp l
    promoteExp exp = case exp of
      App l1 (Lambda l2 p e1) e2  -> App l1 (Lambda l2 p (promoteExp e1)) e2
      _ -> Pr (ann exp) exp