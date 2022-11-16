module Translation.NameResolution where

import Prelude
import Data.Map (Map, fromList, mapWithKey, toList)
import qualified Data.Map as M

import Language.Haskell.Names

import Language.Absyn

import Syntax.Common (VarName(..), ModName(..), reservedOps)

nameResolve :: Map ModName [VarName] -> Module SrcSpanInfo -> Module SrcSpanInfo
nameResolve importedSymbols mod =
  let env :: Environment
      env = mkEnv importedSymbols
      scoped = annotate env mod
      resolved = resolveModule scoped
  in resolved
  where
    mkEnv :: Map ModName [VarName] -> Environment
    mkEnv importedSymbols = fromList $ map
      (\(mn, vns) ->
          ( ModuleName () mn
          , map (\vn -> Value (ModuleName () mn) (Ident () vn)) vns))
      (toList importedSymbols)

    -- 予約二項演算子はNot in Scopeになるのが正しい挙動のよう。
    -- reserved = map
    --     (\(mn, vns) ->
    --       ( ModuleName () mn
    --       , map (\vn -> Value (ModuleName () mn) (Symbol () vn)) vns))
    --     [("Base", reservedOps)]

dsc :: Scoped SrcSpanInfo -> SrcSpanInfo
dsc (Scoped _ loc) = loc

resolveModule :: Module (Scoped SrcSpanInfo) -> Module SrcSpanInfo
resolveModule (Module l mmh pragmas imps decls) =
  Module (dsc l) (fmap (fmap dsc) mmh) (map (fmap dsc) pragmas) (map (fmap dsc) imps) (map resolveDecl decls)

resolveDecl :: Decl (Scoped SrcSpanInfo) -> Decl SrcSpanInfo
resolveDecl decl = case decl of
  FunBind sl ms        -> FunBind (dsc sl) $ map resolveMatch ms
  PatBind sl p rhs mbs -> PatBind (dsc sl) (fmap dsc p) (resolveRhs rhs) (fmap resolveBinds mbs)

resolveBinds :: Binds (Scoped SrcSpanInfo) -> Binds SrcSpanInfo
resolveBinds (BDecls sl decls) = BDecls (dsc sl) (map resolveDecl decls)

resolveRhs :: Rhs (Scoped SrcSpanInfo) -> Rhs SrcSpanInfo
resolveRhs (UnGuardedRhs sl exp) = UnGuardedRhs (dsc sl) (resolveExp exp)

resolveAlt :: Alt (Scoped SrcSpanInfo) -> Alt SrcSpanInfo
resolveAlt (Alt sl p rhs mbs) = Alt (dsc sl) (fmap dsc p) (resolveRhs rhs) (fmap resolveBinds mbs)

resolveMatch :: Match (Scoped SrcSpanInfo) -> Match SrcSpanInfo
resolveMatch m = case m of
  Match sl n ps rhs mbs        -> Match (dsc sl) (fmap dsc n) (map (fmap dsc) ps) (resolveRhs rhs) (fmap resolveBinds mbs)
  InfixMatch sl p n ps rhs mbs -> InfixMatch (dsc sl) (fmap dsc p) (fmap dsc n) (map (fmap dsc) ps) (resolveRhs rhs) (fmap resolveBinds mbs)

resolveExp :: Exp (Scoped SrcSpanInfo) -> Exp SrcSpanInfo
resolveExp exp = case exp of
  Var sl1 (UnQual (Scoped (GlobalSymbol sym _) l) name) -> 
    let mn = ModuleName l (getName $ symbolModule sym)
    in Var (dsc sl1) (Qual l mn (fmap dsc name))
  Var sl1 qn            -> Var (dsc sl1) (fmap dsc qn)
  Lit sl l              -> Lit (dsc sl) (fmap dsc l)
  InfixApp sl e1 qop e2 -> InfixApp (dsc sl) (resolveExp e1) (fmap dsc qop) (resolveExp e2)
  App sl e1 e2          -> App (dsc sl) (resolveExp e1) (resolveExp e2)
  Tuple sl b lst        -> Tuple (dsc sl) b (map resolveExp lst)
  List sl lst           -> List (dsc sl) (map resolveExp lst)
  If sl e1 e2 e3        -> If (dsc sl) (resolveExp e1) (resolveExp e2) (resolveExp e3)
  Case sl e alts        -> Case (dsc sl) (resolveExp e) (map resolveAlt alts)
  Lambda sl p e         -> Lambda (dsc sl) (map (fmap dsc) p) (resolveExp e)
  Let sl bs e           -> Let (dsc sl) (resolveBinds bs) (resolveExp e)
  VRes sl vbs e         -> VRes (dsc sl) (fmap dsc vbs) (resolveExp e)
  VExt sl e             -> VExt (dsc sl) (resolveExp e)
  Paren sl e            -> Paren (dsc sl) (resolveExp e)
  _ -> error $ "\nUnsupported exp in resolveExp\n" ++ show exp