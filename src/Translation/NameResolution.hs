module Translation.NameResolution where

import Prelude
import Data.Map (Map, fromList, mapWithKey, toList)
import qualified Data.Map as M

import Language.Haskell.Names

import Language.Absyn

import Syntax.Common (VarName(..), ModName(..), reservedOps)

import Util hiding (annotate)
import Control.Monad (when)
import Data.Maybe (fromMaybe)

nameResolve :: Map ModName [VarName] -> Module SrcSpanInfo -> IO (Module SrcSpanInfo)
nameResolve importedSymbols mod = do
  -- mod' <- qualifyRecursive mod
  let env :: Environment
      env = mkEnv importedSymbols
      scoped = annotate env mod
      resolved = resolveModule scoped
  return resolved
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
  -- _ -> error ""

resolveModule :: Module (Scoped SrcSpanInfo) -> Module SrcSpanInfo
resolveModule (Module l mmh pragmas imps decls) =
  Module
    (dsc l)
    (fmap (fmap dsc) mmh)
    (map (fmap dsc) pragmas)
    (map (fmap dsc) imps)
    (map resolveDecl decls)

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
  -- UnQualの変数のみをQualに解決する。SpecialConはScopeErrorでも無視
  Var sl1 qn -> case qn of
    UnQual (Scoped nameInfo l) n -> case nameInfo of
      GlobalSymbol sym qn ->
        let ModuleName _ mns = symbolModule sym
            mn = ModuleName l mns in
        Var (dsc sl1) (Qual l mn (fmap dsc n))
      LocalValue loc -> Var (dsc sl1) (UnQual l (fmap dsc n))
      ScopeError err -> error $ "ScopeError : " ++ show err
      None           -> error "NameInfo : None"
      _              -> error "NameInfo : uncatched error"
    Qual (Scoped nameInfo l) mn name -> case nameInfo of
      _ -> Var (dsc sl1) $ Qual l (fmap dsc mn) (fmap dsc name)
    -- let Qual _ (ModuleName _ mns) _ = qn
    -- in Var (dsc sl1) (Qual l (ModuleName l mns) (fmap dsc n))

  -- Qualはスコープ内そのシンボルが存在するかチェックする
  -- Var sl1 (Qual sc mn name) -> case sc of
  --   Scoped (GlobalSymbol sym _) l ->
  --     let mn' = ModuleName l (getName $ symbolModule sym)
  --     in Var (dsc sl1) (Qual l mn' (fmap dsc name))
  --   Scoped (ScopeError err) l -> scopeError err
  --   _                         -> error "UndefinedError occurs in Name resolution"

  NegApp sl e           -> NegApp (dsc sl) (resolveExp e)
  Lit sl l              -> Lit (dsc sl) (fmap dsc l)
  InfixApp sl e1 qop e2 -> InfixApp (dsc sl) (resolveExp e1) (fmap dsc qop) (resolveExp e2)
  App sl e1 e2          -> App (dsc sl) (resolveExp e1) (resolveExp e2)
  Tuple sl b lst        -> Tuple (dsc sl) b (map resolveExp lst)
  List sl lst           -> List (dsc sl) (map resolveExp lst)
  If sl e1 e2 e3        -> If (dsc sl) (resolveExp e1) (resolveExp e2) (resolveExp e3)
  Case sl e alts        -> Case (dsc sl) (resolveExp e) (map resolveAlt alts)
  Lambda sl p e         -> Lambda (dsc sl) (map (fmap dsc) p) (resolveExp e)
  Let sl bs e           -> Let (dsc sl) (resolveBinds bs) (resolveExp e)
  Con sl qn             -> Con (dsc sl) (fmap dsc qn)
  VRes sl vbs e         -> VRes (dsc sl) (fmap dsc vbs) (resolveExp e)
  VExt sl e             -> VExt (dsc sl) (resolveExp e)
  Paren sl e            -> Paren (dsc sl) (resolveExp e)
  _ -> error $ "\nUnsupported exp in resolveExp\n" ++ show exp

scopeError :: (Show l, PrettyAST l) => Error l -> a
scopeError err = case err of
  ENotInScope qn -> error $ putDocString $ ppP "name is not in scope" <> line <> ppP qn
  EAmbiguous qn syms -> error "name is ambiguous"
  ENotExported mbn n mn -> error "Attempt to explicitly import a name which is not exported (or, possibly, does not even exist). For example:"
  EModNotFound mn -> error $ putDocString $ line <> ppP "Module not found" <> line <> ppP mn
  EInternal str   -> error str
  _               -> error "scopeError: Undefined error occurs in nameresolution"

-- トップレベルシンボルに重複がないかを検査する
duplicatedTopSyms ::  Module l -> IO ()
duplicatedTopSyms mod = do
  let ss = getTopSyms mod
  duplicatedTopSyms' (getDecls mod) []
  where
    duplicatedTopSyms' :: [Decl l] -> [VarName] -> IO ()
    duplicatedTopSyms' []     _     = return ()
    duplicatedTopSyms' (d:ds) found = do
      let n = getName d
      when (n `elem` found) $ error $ "Duplicated symbols found : " ++ n
      duplicatedTopSyms' ds found

-- qualifyRecursive :: (PrettyAST l, Show l) => Module l -> IO (Module l)
-- qualifyRecursive (Module l mmh pragmas imps decls) = do
--   let mn = getName $ fromMaybe (error "No module name") mmh
--   decls' <- mapM (qualifyRecursiveDecl mn []) decls
--   let m' = Module l mmh pragmas imps decls'
--   return m'
--   where
--     qualifyRecursiveDecl :: (PrettyAST l, Show l) => String -> [String] -> Decl l -> IO (Decl l)
--     qualifyRecursiveDecl mn pns decl = case decl of
--       FunBind l ms        -> FunBind l <$> mapM (qualifyRecursiveMatch mn pns) ms
--       PatBind l p rhs mbs -> do let pn = case p of
--                                           PVar _ n -> getName p
--                                           p        -> error $ unlines [
--                                                         "Unexpected pattern."
--                                                       , "pattern : " ++ show p
--                                                       , "rhs     : " ++ show rhs 
--                                                       , "mbs     : " ++ show mbs
--                                                       ]
--                                 PatBind l p
--                                   <$> qualifyRecursiveRhs mn (pn:pns) rhs
--                                   <*> mapM (qualifyRecursiveBinds mn (pn:pns)) mbs
 

--     qualifyRecursiveMatch mn pns m = case m of
--       Match l n ps rhs mbs        -> do let pns' = getName n : pns
--                                         Match l n ps
--                                           <$> qualifyRecursiveRhs mn pns' rhs
--                                           <*> mapM (qualifyRecursiveBinds mn pns') mbs
--       InfixMatch l p n ps rhs mbs -> do let pns' = getName n : pns
--                                         InfixMatch l p n ps
--                                           <$> qualifyRecursiveRhs mn pns' rhs
--                                           <*> mapM (qualifyRecursiveBinds mn pns') mbs

--     qualifyRecursiveBinds mn pns (BDecls l decls) = BDecls l <$> mapM (qualifyRecursiveDecl mn pns) decls

--     qualifyRecursiveRhs mn pns (UnGuardedRhs l exp) = UnGuardedRhs l <$> qualifyRecursiveExp mn pns exp

--     qualifyRecursiveAlt mn pns (Alt l p rhs mbs) = Alt l p
--                                                     <$> qualifyRecursiveRhs mn pns rhs
--                                                     <*> mapM (qualifyRecursiveBinds mn pns) mbs

--     qualifyRecursiveExp :: (PrettyAST l, Show l) => String -> [String] -> Exp l -> IO (Exp l)
--     qualifyRecursiveExp mn pns exp = case exp of
--       Var l qn             -> let qn' = case qn of
--                                     UnQual l n  -> if getName n `elem` pns
--                                                     then Qual l (ModuleName l mn) n
--                                                     else UnQual l n
--                                     Qual l mn n -> Qual l mn n in
--                               return $ Var l qn'
--       NegApp l e           -> NegApp l <$> qualifyRecursiveExp mn pns e
--       Lit l lit            -> return $ Lit l lit
--       InfixApp l e1 qop e2 -> InfixApp l
--                                 <$> qualifyRecursiveExp mn pns e1
--                                 <*> return qop
--                                 <*> qualifyRecursiveExp mn pns e2
--       App l e1 e2          -> App l
--                                 <$> qualifyRecursiveExp mn pns e1
--                                 <*> qualifyRecursiveExp mn pns e2
--       Tuple l b lst        -> Tuple l b <$> mapM (qualifyRecursiveExp mn pns) lst
--       List l lst           -> List l <$> mapM (qualifyRecursiveExp mn pns) lst
--       If l e1 e2 e3        -> If l
--                                 <$> qualifyRecursiveExp mn pns e1
--                                 <*> qualifyRecursiveExp mn pns e2
--                                 <*> qualifyRecursiveExp mn pns e3
--       Case l e alts        -> Case l
--                                 <$> qualifyRecursiveExp mn pns e
--                                 <*> mapM (qualifyRecursiveAlt mn pns) alts
--       Lambda l p e         -> Lambda l p <$> qualifyRecursiveExp mn pns e
--       Let l bs e           -> Let l
--                                 <$> qualifyRecursiveBinds mn pns bs
--                                 <*> qualifyRecursiveExp mn pns e
--       Con l qn             -> return $ Con l qn
--       VRes l vbs e         -> VRes l vbs <$> qualifyRecursiveExp mn pns e
--       VExt l e             -> VExt l <$> qualifyRecursiveExp mn pns e
--       Paren l e            -> Paren l <$> qualifyRecursiveExp mn pns e
--       _                    -> error $ "\nUnsupported exp in resolveExp\n" ++ show exp