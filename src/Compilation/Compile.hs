module Compilation.Compile where

import Data.Map
import Data.Set hiding (insert)
import Data.List (find, map, filter, null, foldl)
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.State

import Syntax.Absyn (getImports, Module (..))
import Syntax.Env hiding (setCounter, counter)
import Syntax.SrcLoc (SrcSpanInfo)
import Syntax.Label

import Translation.Desugar (desugarAST)
import Translation.Girard (girardFwd)
import Inference.TypeInference (getInterface, TypedExp (..))
import Compilation.Bundling

import DependencyGraph
import Util
import Syntax.Type (Type(..), Constraints(..), landC)
import Syntax.Version (Version(Root))
import Translation.Promote
import Translation.Normalize (normalize)
import Translation.RenameExVars (renameExVarModule, duplicateEnvs, CounterTable)
import qualified Data.IntMap as M

type BundledTEnv = Map String TEnv
type BundledUEnv = Map String UEnv
type BundledConstraints = Map String Constraints


data CompileEnv' = CompileEnv'
  { pending :: [VLMod]  -- コンパイル待ちのモジュール、先頭からコンパイルされる
  , counter :: Int      -- 型変数生成用の通し番号
  , counterTable :: CounterTable -- 複製された外部モジュール変数の登場回数の通し番号
  , mapParsedAST :: Map VLMod (Module SrcSpanInfo) -- パース済みのAST
  , globalEnv :: Map VLMod [TypedExp] -- 型検査後の型情報をそのまま格納するグローバル環境
  , globalConstraints :: Constraints -- 全ての制約が入っているグローバル制約環境
  , bundledTEnv :: BundledTEnv -- バンドル後の型情報が格納されている型環境
  , bundledUEnv :: BundledUEnv -- バンドル後の型変数情報が格納されている環境
  , bundledConstraints :: BundledConstraints -- bundledTEnvに対応する制約が格納されている環境
  , bundledConsSchemes :: Map String (Map String Constraints) -- 各モジュールの各シンボルのバンドル後の制約（後で複製される）
  , logFilePath :: FilePath  -- ログファイルの相対パス
  }
  deriving (Show)
type CompileEnv a = StateT CompileEnv' IO a

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

getParsedAST :: VLMod -> CompileEnv (Module SrcSpanInfo)
getParsedAST vlmod = do
  mapParsedAST <- gets mapParsedAST
  let message = show vlmod ++ " cannot be found in " ++ show (keys mapParsedAST)
  return $ fromMaybe (error message) $ Data.Map.lookup vlmod mapParsedAST

addGlobalEnv :: VLMod -> [TypedExp] -> CompileEnv ()
addGlobalEnv vlmod tyexp = state $ \env -> ((), env { globalEnv = insert vlmod tyexp $ globalEnv env })

addGlobalConstraints :: Constraints -> CompileEnv ()
addGlobalConstraints cs = modify $ \env -> env { globalConstraints = cs `landC` globalConstraints env }

setGlobalConstraints :: Constraints -> CompileEnv ()
setGlobalConstraints cs = modify $ \env -> env { globalConstraints = cs }

addBundledTEnv :: String -> TEnv -> CompileEnv ()
addBundledTEnv s tenv = state $ \env -> ((), env { bundledTEnv = insert s tenv $ bundledTEnv env })

addBundledUEnv :: String -> UEnv -> CompileEnv ()
addBundledUEnv s uenv = state $ \env -> ((), env { bundledUEnv = insert s uenv $ bundledUEnv env })

addBundledConstraints :: String -> Constraints -> CompileEnv ()
addBundledConstraints s c = state $ \env -> ((), env { bundledConstraints = insert s c $ bundledConstraints env })

addBundledConsSchemes :: String -> Map String Constraints -> CompileEnv ()
addBundledConsSchemes mn schs = state $ \env -> ((), env { bundledConsSchemes = insert mn schs $ bundledConsSchemes env })

genEnvFromImports :: [String] -> CompileEnv (TEnv, UEnv)
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
    genEnvFromImports' :: [String] -> CompileEnv [(TEnv, UEnv)]
    genEnvFromImports' [] = return []
    genEnvFromImports' (mn:lst) = do
      tenv' <- gets ((! mn) . bundledTEnv)
      uenv' <- gets ((! mn) . bundledUEnv)
      -- cs <- gets ((! mn) . bundledConstraints)
      -- cschm <- gets $ (! mn) . bundledConsSchemes
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

compile :: VLMod -> CompileEnv ()
compile target@(VLMod mn v) = do
  logP "\n==================================="
  logPD $ ppP "=== Start compiling module" <+> ppP target
  logP "==================================="
  ast <- getParsedAST target
  let importMods = getImports ast

  logP "\n=== AST (Syntax.Absyn) ==="
  logP ast

  -- log "=== Alpha renaming ==="
  -- let ast_renamed = alphaRename ast
  -- log ast_renamed

  logP "\n=== Desugared AST (Syntax.Desugared) ==="
  let ast_desugared = desugarAST ast
  logP ast_desugared

  logP "\n=== Normalized AST (Syntax.Desugared) ==="
  let ast_normalized = normalize ast_desugared
  logP ast_normalized

  logP "\n=== AST (Syntax.VL) ==="
  let astVL = promoteTopVal $ girardFwd ast_normalized
  logP astVL

  logP "\n=== AST (Syntax.VL), after duplicating external variables ==="
  ct <- gets counterTable
  let (astVLDuplicated, ct') = renameExVarModule ct astVL
      ctdiff = unionWith (-) ct' ct
      ctstart = unionWith min ct' (Data.Map.union ct $ Data.Map.map (const 0) ct') -- ct'のkeyだけ先に入れておく
  logP astVLDuplicated
  logPD $ (ppP "[DEBUG] ct     : " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ct
  logPD $ (ppP "[DEBUG] ct'    : " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ct'
  logPD $ (ppP "[DEBUG] ctdiff : " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ctdiff
  logPD $ (ppP "[DEBUG] ctstart: " <>) $ concatWith (surround $ comma <> space) $ mapWithKey (\k v -> ppP k <> ppP "->" <> ppP v) ctstart
  setCounterTable ct'


  logP "\n=== Importing Exteranal ModulesType (Syntax.VL) ==="
  (importedTEnv, importedUEnv) <- genEnvFromImports importMods
  let flattenSchemeLst :: [Map String Constraints] -> Map String Constraints
      flattenSchemeLst [] = Data.Map.empty
      flattenSchemeLst (h:rst) = unionWith (error "duplicated key included!") h (flattenSchemeLst rst)
  consSchemes <- flattenSchemeLst <$> forM importMods (\mn -> gets $ (! mn) . bundledConsSchemes)
  -- lift $ print consSchemes
  -- [TODO] A.fとB.fを同時にimport出来なくなっているので名前解決をちゃんとやる必要がある
  gcs <- gets globalConstraints
  logPD $ ppP "[DEBUG] Imported TEnv      :" <+> ppP importedTEnv    -- import宣言から得られた型環境
  logPD $ ppP "[DEBUG] Imported UEnv      :" <+> ppP importedUEnv    -- import宣言から得られた型変数環境
  logPD $ ppP "[DEBUG] global constraints :" <+> ppP gcs <> line

  logP "=== Duplicate External Symbols (Syntax.VL) ==="
  let (tenvDuplicated, uenvDuplicated, consDuplicated) = duplicateEnvs ctdiff ctstart (importedTEnv, importedUEnv, consSchemes)
  addGlobalConstraints consDuplicated
  logPD $ ppP "[DEBUG] Duplicated Imported TEnv        :" <+> ppP tenvDuplicated
  logPD $ ppP "[DEBUG] Duplicated Imported UEnv        :" <+> ppP uenvDuplicated
  logPD $ ppP "[DEBUG] Duplicated Imported Constraints :" <+> ppP consDuplicated <> line

  initCounter <- gets counter
  logPD $ ppP "[DEBUG] Initial Counter     :" <+> ppP initCounter <> line    -- 型変数生成用の通し番号

  logP "=== Type Inference (Syntax.VL) ==="
  let (typedExpsWithLogs, c) = getInterface tenvDuplicated uenvDuplicated initCounter astVLDuplicated -- 型推論
  setCounter c
  forM_ typedExpsWithLogs logP -- 型推論木付き
  logP ""

  logP "=== Inferred Types (Syntax.VL) ==="
  let typedExps = Data.List.map fst typedExpsWithLogs
      consOfTarget = foldl1 landC $ Prelude.map (\(TypedExp _ _ con _)  -> con) typedExps
  forM_ typedExps logP -- 型推論木付き
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
    addBundledConstraints mn newCons
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
    Just res -> compile res

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

instance PrettyAST BundledConstraints where
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