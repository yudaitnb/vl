module Compile where

import Data.Map
import Data.List (find, map, filter, null, foldl)
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.State

import qualified Language.Absyn as AB (getImports, Module (..), getTopSyms)
import qualified Language.LambdaVL as VL (Exp (..), Decl (..), getDecls, splitDeclsToMap)

import Syntax.Common

import Syntax.Env hiding (setCounter, counter)
import Syntax.Type (Type(..), Constraints(..), landC)

import Translation.NameResolution ( nameResolve, duplicatedTopSyms )
import Translation.Desugar (desugarAST)
import Translation.Girard (girardFwd)
import Translation.Promote ( promoteTopVal )
import Translation.Normalize (normalize)
import Translation.DuplicateExVars (duplicateExVarModule, duplicateEnvs, CounterTable)
import Translation.Bundling ( bundle )

import Inference.TypeInference (getInterface, TypedExp (..))

import Parser ( VLMod(..), ParsedAST )
import Util

type BundledTEnv = Map ModName TEnv
type BundledUEnv = Map ModName UEnv
type DuplicatedExVars = Map VarKey (VarKey, VLMod, Type)

type VLDecls = Map VarName (VL.Exp SrcSpanInfo)
type VLModDecls = Map VLMod VLDecls

data CompileEnv' = CompileEnv'
  { pending :: [VLMod]  -- コンパイル待ちのモジュール、先頭からコンパイルされる
  , counter :: Int      -- 型変数生成用の通し番号
  , counterTable :: CounterTable -- 複製された外部モジュール変数の登場回数の通し番号
  , mapParsedAST :: ParsedAST -- パース済みのAST
  , mapVLDecls :: VLModDecls -- 全ての変換処理を終えたDecls -- [TODO] 複数のモジュールで同一変数をちゃんと処理できるようにする
  , globalEnv :: Map VLMod [TypedExp] -- 型検査後の型情報をそのまま格納するグローバル環境
  , globalConstraints :: Constraints -- 全ての制約が入っているグローバル制約環境
  , bundledTEnv :: BundledTEnv -- モジュール毎のバンドル後の型情報が格納されている型環境
  , bundledUEnv :: BundledUEnv -- モジュール毎のバンドル後の型変数情報が格納されている環境
  , bundledConsSchemes :: Map ModName (Map VarName Constraints) -- 各モジュールの各シンボルのバンドル後の制約（後で複製される）
  , duplicatedExVars :: DuplicatedExVars -- 複製された外部変数と(元の名前, 複製されたモジュール, リソース変数)のマップ, duplicationフェーズで保存される
  , logFilePath :: FilePath  -- ログファイルの相対パス
  }
  deriving (Show)
type CompileEnv a = StateT CompileEnv' IO a

initCompileEnv :: CompileEnv'
initCompileEnv = CompileEnv'
  []    -- pending
  0     -- counter
  empty -- counterTable
  empty -- mapParsedAST
  empty -- mapVLDecls
  empty -- globalEnv
  CTop  -- globalConstraints
  empty -- bundledTEnv
  empty -- bundledUEnv
  empty -- bundledConsSchemes
  empty -- duplicatedExVars
  ""    -- logFilePath

addCounter :: Int -> CompileEnv ()
addCounter i = modify $ \env -> env { counter = i + counter env }

setCounter :: Int -> CompileEnv ()
setCounter i = modify $ \env -> env { counter = i }

setCounterTable :: CounterTable -> CompileEnv ()
setCounterTable ct = modify $ \env -> env { counterTable = ct }

getNextModule :: CompileEnv (Maybe VLMod)
getNextModule = state $ \env -> case pending env of
  []  -> (Nothing, env)
  lst -> (Just $ head lst, env { pending = tail lst })

getParsedAST :: VLMod -> CompileEnv (AB.Module SrcSpanInfo)
getParsedAST vlmod = do
  mapParsedAST <- gets mapParsedAST
  let message = show vlmod ++ " cannot be found in " ++ show (keys mapParsedAST)
  return $ fromMaybe (error message) $ Data.Map.lookup vlmod mapParsedAST

addVLDecls :: VLMod -> [VL.Decl SrcSpanInfo] -> CompileEnv ()
addVLDecls vlmod decls = modify $ \env -> 
  let oldDeclsOfVlMod = mapVLDecls env
      newDeclsOfVlMod = singleton vlmod (VL.splitDeclsToMap decls) `union` oldDeclsOfVlMod
  in env { mapVLDecls = newDeclsOfVlMod }

addGlobalEnv :: VLMod -> [TypedExp] -> CompileEnv ()
addGlobalEnv vlmod tyexp = modify $ \env -> env { globalEnv = insert vlmod tyexp $ globalEnv env }

addGlobalConstraints :: Constraints -> CompileEnv ()
addGlobalConstraints cs = modify $ \env -> env { globalConstraints = cs `landC` globalConstraints env }

setGlobalConstraints :: Constraints -> CompileEnv ()
setGlobalConstraints cs = modify $ \env -> env { globalConstraints = cs }

addBundledTEnv :: ModName -> TEnv -> CompileEnv ()
addBundledTEnv mn tenv = modify $ \env -> env { bundledTEnv = insert mn tenv $ bundledTEnv env }

addBundledUEnv :: ModName -> UEnv -> CompileEnv ()
addBundledUEnv s uenv = modify $ \env -> env { bundledUEnv = insert s uenv $ bundledUEnv env }

addBundledConsSchemes :: ModName -> Map VarName Constraints -> CompileEnv ()
addBundledConsSchemes mn schs = modify $ \env -> env { bundledConsSchemes = insert mn schs $ bundledConsSchemes env }

addDuplicatedExVars :: VLMod -> ExVarResources -> CompileEnv ()
addDuplicatedExVars vlmod exr = modify $ \env -> 
  let oldDuplicatedExVarsOfVlMod = duplicatedExVars env
      newDuplicatedExVarsOfVlMod = Data.Map.map (\(orig, tv) -> (orig, vlmod, tv)) exr `union` oldDuplicatedExVarsOfVlMod
  in env { duplicatedExVars = newDuplicatedExVarsOfVlMod }

getImportedSymbols :: [ModName] -> CompileEnv (Map ModName [VarName])
getImportedSymbols mods = do
  lst <- forM mods $ \mn -> do
    vnsOfMn <- gets (keys . (<!> mn) . bundledTEnv)
    let vnsOfMn' = Data.List.map getName vnsOfMn
    return (mn, vnsOfMn')
  return $ fromList lst

genEnvFromImports :: [ModName] -> CompileEnv (TEnv, UEnv)
genEnvFromImports modules = do
  envs <- genEnvFromImports' modules
  let (newTEnv, newUEnv) = Data.List.foldl
                            (\(accTEnv, accUEnv) (tenv,uenv) ->
                              ( accTEnv .++. tenv
                              , accUEnv `Data.Map.union` uenv))
                            ( emptyEnv :: TEnv
                            , emptyEnv :: UEnv)
                            envs
  return (newTEnv, newUEnv)
  where
    genEnvFromImports' :: [ModName] -> CompileEnv [(TEnv, UEnv)]
    genEnvFromImports' [] = return []
    genEnvFromImports' (mn:lst) = do
      tenv' <- gets ((<!> mn) . bundledTEnv)
      uenv' <- gets ((<!> mn) . bundledUEnv)
      -- cschm <- gets $ (<!> mn) . bundledConsSchemes
      xs <- genEnvFromImports' lst
      return $ (tenv', uenv') : xs

isLastVersion :: VLMod -> CompileEnv Bool
isLastVersion mod = do
  pending <- gets pending
  let VLMod mn _ = mod
      sameMN = Data.List.filter (\(VLMod mn' _) -> mn' == mn) pending
  return $ Data.List.null sameMN

logP :: PrettyAST a => a -> CompileEnv ()
logP s = do
  logfile <- gets logFilePath
  lift $ logPpLn ppP logfile s

logPD :: Doc a -> CompileEnv ()
logPD s = do
  logfile <- gets logFilePath
  lift $ logPpLn ppP logfile $ putDocString s

logE :: PrettyAST a => a -> CompileEnv ()
logE s = do
  logfile <- gets logFilePath
  lift $ logPpLn ppE logfile s

--------------

compile :: [VLMod] -> Map VLMod (AB.Module SrcSpanInfo) -> FilePath -> IO CompileEnv'
compile sortedVLMods mapParsedAst logFilePath = do
  let headVLMod  = head sortedVLMods
      tailVLMods = tail sortedVLMods
      initCompEnv = initCompileEnv {
                      pending = tailVLMods
                    , mapParsedAST = mapParsedAst
                    , logFilePath = logFilePath
                    }
  execStateT (compileVLMod headVLMod) initCompEnv

compileVLMod :: VLMod -> CompileEnv ()
compileVLMod target@(VLMod mn v) = do
  logP "\n==================================="
  logPD $ ppP "=== Start compiling module" <+> ppP target
  logP "==================================="
  ast <- getParsedAST target
  let importMods = AB.getImports ast

  logP "\n=== AST (Syntax.Absyn) ==="
  logP ast

  logP "=== Name resolution ==="
  liftIO $ duplicatedTopSyms ast
  importedSymbols <- getImportedSymbols importMods
  let ast_resolved = nameResolve importedSymbols ast
  logP ast_resolved

  logP "\n=== Desugared AST (Syntax.Desugared) ==="
  let ast_desugared = desugarAST ast_resolved
  logP ast_desugared

  logP "\n=== Normalized AST (Syntax.Desugared) ==="
  let ast_normalized = normalize ast_desugared
  logP ast_normalized

  logP "\n=== AST (Syntax.VL) ==="
  let astVL = girardFwd ast_normalized
  logP astVL

  logP "\n=== AST (Syntax.VL), after duplicating external variables ==="
  ct <- gets counterTable
  let (astVLDuplicated, ct') = duplicateExVarModule ct astVL
      ctdiff = unionWith (-) ct' ct
      ctstart = unionWith min ct' (Data.Map.union ct $ Data.Map.map (const 0) ct') -- ct'のkeyだけ先に入れておく
  logP astVLDuplicated
  logPD $ (ppP "[DEBUG] ct     : " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ct
  logPD $ (ppP "[DEBUG] ct'    : " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ct'
  logPD $ (ppP "[DEBUG] ctdiff : " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ctdiff
  logPD $ (ppP "[DEBUG] ctstart: " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ctstart
  setCounterTable ct'
  addVLDecls target $ VL.getDecls astVLDuplicated

  logP "\n=== Importing Exteranal ModulesType (Syntax.VL) ==="
  let flattenSchemeLst :: [Map VarName Constraints] -> Map VarName Constraints
      flattenSchemeLst []      = Data.Map.empty
      flattenSchemeLst (h:rst) = unionWith (error "duplicated key included!") h (flattenSchemeLst rst)
  consSchemes <- flattenSchemeLst <$> forM importMods (\mn -> gets $ (<!> mn) . bundledConsSchemes)
  -- lift $ print consSchemes
  -- [TODO] A.fとB.fを同時にimport出来なくなっているので名前解決をちゃんとやる必要がある
  (importedTEnv, importedUEnv) <- genEnvFromImports importMods
  logPD $ ppP "[DEBUG] Imported TEnv      :" <+> ppP importedTEnv    -- import宣言から得られた型環境
  logPD $ ppP "[DEBUG] Imported UEnv      :" <+> ppP importedUEnv    -- import宣言から得られた型変数環境

  logP "=== Duplicate External Symbols (Syntax.VL) ==="
  let (tenvDuplicated, uenvDuplicated, consDuplicated, exVarResourcesDuplicated) = duplicateEnvs ctdiff ctstart (importedTEnv, importedUEnv, consSchemes)
  logPD $ ppP "[DEBUG] Duplicated Imported TEnv        :" <+> ppP tenvDuplicated
  logPD $ ppP "[DEBUG] Duplicated Imported UEnv        :" <+> ppP uenvDuplicated
  logPD $ ppP "[DEBUG] Duplicated Imported Constraints :" <+> ppP consDuplicated <> line
  addGlobalConstraints consDuplicated
  addDuplicatedExVars target exVarResourcesDuplicated
  logPD $ ppP "[DEBUG] add duplicated constraints                 : " <> line <> ppP consDuplicated
  logPD $ ppP "[DEBUG] add duplicated external variable resources : " <> line <> ppP exVarResourcesDuplicated <> line

  initCounter <- gets counter
  logPD $ ppP "[DEBUG] Initial Counter     :" <+> ppP initCounter <> line    -- 型変数生成用の通し番号

  logP "=== Type Inference (Syntax.VL) ==="
  (typedExpsWithLogs, c) <- liftIO $ getInterface tenvDuplicated uenvDuplicated initCounter astVLDuplicated -- 型推論
  setCounter c
  forM_ typedExpsWithLogs logP -- 型推論木付き
  logP ""

  logP "=== Inferred Types (Syntax.VL) ==="
  let typedExps = Data.List.map fst typedExpsWithLogs
      consOfTarget = foldl1 landC $ Prelude.map (\(TypedExp _ _ con _)  -> con) typedExps
  forM_ typedExps logP -- 型推論の結果のみ
  logP ""

  addGlobalEnv target typedExps
  logPD $ ppP "[DEBUG] Add global env :" <+> ppP typedExps
  addGlobalConstraints consOfTarget
  logPD $ ppP "[DEBUG] Add global constraints :" <+> ppP consOfTarget

  tempCounter <- gets counter
  logPD $ ppP "[DEBUG] Counter:" <+> ppP tempCounter

  isLastVersion target >>= \b -> when (b && v /= Root) $ do
    logP "\n============================================"
    logPD $ ppP "=== Bundling top-level symbols of module" <+> ppP mn
    logP "============================================"
    c <- gets counter
    resultMap <- gets (Data.Map.filterWithKey (\(VLMod mn' _) _ -> mn' == mn) . globalEnv)
    let (newTEnv, newUEnv, newCons, newScheme, newCounter) = bundle mn c resultMap

    addBundledTEnv mn newTEnv
    logPD $ ppP "[DEBUG] Add bundled tenv        :" <+> ppP newTEnv
    addBundledUEnv mn newUEnv
    logPD $ ppP "[DEBUG] Add bundled uenv        :" <+> ppP newUEnv
    addGlobalConstraints newCons
    logPD $ ppP "[DEBUG] Add bundled/global constraints :" <+> ppP newCons
    addBundledConsSchemes mn newScheme
    logPD $ ppP "[DEBUG] Add bundled constraints schemes :" <+> ppP newScheme
    setCounter newCounter
    logPD $ ppP "[DEBUG] Counter     :" <+> ppP newCounter

  pendingMods <- gets pending
  gcs <- gets globalConstraints
  bcs <- gets bundledConsSchemes
  logPD $ ppP "[DEBUG] globalConstraints:"
  logPD $ ppP gcs <> line
  logPD $ ppP "[DEBUG] bundledConsSchemes:"
  logPD $ concatWith (surround $ comma <> space) (Data.List.map ppP $ Data.Map.toList bcs) <> line
  logPD $ ppP "[DEBUG] List of modules not yet compiled:"
  logPD $ ppP pendingMods <> line

  -- ^ Proceed to the next compile target
  getNextModule >>= \case
    Nothing  -> return ()
    Just res -> compileVLMod res

instance PrettyAST BundledTEnv where
  ppE m
    | Data.Map.null m = ppE "{}"
    | otherwise = concatWith (surround $ comma <> space) $
      Data.List.map (\(k,v) -> parens $ ppE k <> comma <> ppE v) $
      Data.Map.toList m
  ppP m 
    | Data.Map.null m = ppP "{}"
    | otherwise = concatWith (surround $ comma <> space) $
      Data.List.map (\(k,v) -> parens $ ppP k <> comma <> ppP v) $
      Data.Map.toList m

instance PrettyAST BundledUEnv where
  ppE m
    | Data.Map.null m = ppE "{}"
    | otherwise = concatWith (surround $ comma <> space) $
      Data.List.map (\(k,v) -> parens $ ppE k <> comma <> ppE v) $
      Data.Map.toList m
  ppP m 
    | Data.Map.null m = ppP "{}"
    | otherwise = concatWith (surround $ comma <> space) $
      Data.List.map (\(k,v) -> parens $ ppP k <> comma <> ppP v) $
      Data.Map.toList m

instance PrettyAST (Map VarName Constraints) where
  ppE m
    | Data.Map.null m = ppE "{}"
    | otherwise = concatWith (surround $ comma <> space) $
      Data.List.map (\(k,v) -> parens $ ppE k <> comma <> ppE v) $
      Data.Map.toList m
  ppP m 
    | Data.Map.null m = ppP "{}"
    | otherwise = concatWith (surround $ comma <> space) $
      Data.List.map (\(k,v) -> parens $ ppP k <> comma <> ppP v) $
      Data.Map.toList m

instance PrettyAST VLModDecls where
  ppE m
    | Data.Map.null m = ppE "{}"
    | otherwise = concatWith (surround line) $
      mapWithKey (\vlmod vldecls -> ppE vlmod <+> colon <+> ppE vldecls) m
  ppP m 
    | Data.Map.null m = ppP "{}"
    | otherwise = concatWith (surround $ line <> line) $
      mapWithKey
        (\vlmod vldecls ->
          ppP "### module" <+> ppP vlmod <+> ppP "declarations" <> line <>
          ppP vldecls)
        m

instance PrettyAST VLDecls where
  ppE m
    | Data.Map.null m = ppE "{}"
    | otherwise = concatWith (surround $ comma <> space) $
      mapWithKey (\vn decls -> ppE vn <+> ppP "=" <+> ppE decls) m
  ppP m 
    | Data.Map.null m = ppP "{}"
    | otherwise = concatWith (surround line) $
      mapWithKey (\vn decls -> ppP vn <+> ppP "=" <+> ppP decls) m
