module Translation.Promote where

import Language.LambdaVL
import Syntax.Common

promoteTopVal :: VLMod -> Module l -> Module l
promoteTopVal vlmod (Module l mh pragmas imp decls) =
  let newDecls = case vlmod of
        VLMod "Main" Root -> map promoteDeclMain decls -- Rootモジュールはmainだけ
        _                 -> map promoteDecl decls     -- 他は全declをpromote
  in Module l mh pragmas imp newDecls
  where
    promoteDeclMain :: Decl l -> Decl l
    promoteDeclMain pb@(PatBind l1 pvar exp) =
      if getName pvar == "main"
        then PatBind l1 pvar (promoteExp exp) -- [TODO] not l1
        else pb
      where
        promoteExp :: Exp l -> Exp l
        promoteExp exp = case exp of
          App l1 (Lambda l2 p e1) e2     -> App l1 (Lambda l2 p (promoteExp e1)) e2
          Lambda l1 p (Case l2 exp alts) -> Lambda l1 p (Case l2 exp $ map promoteAlt alts)
          _ -> Pr (ann exp) exp

        promoteAlt :: Alt l -> Alt l
        promoteAlt alt = case alt of
          Alt l p e -> Alt l p $ promoteExp e

    promoteDecl :: Decl l -> Decl l
    promoteDecl pb@(PatBind l1 pvar exp) = PatBind l1 pvar (promoteExp exp) -- [TODO] not l1
      where
        promoteExp :: Exp l -> Exp l
        promoteExp exp = case exp of
          -- App l1 (Lambda l2 p e1) e2     -> App l1 (Lambda l2 p (promoteExp e1)) e2
          _ -> Pr (ann exp) exp
        
        promoteAlt :: Alt l -> Alt l
        promoteAlt alt = case alt of
          Alt l p e -> Alt l p $ promoteExp e