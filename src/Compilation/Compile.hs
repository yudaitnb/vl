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
import Inference.TypeInference (getInterface, TypedExp)
import Compilation.Bundling

import DependencyGraph
import Util
import Syntax.Type (Type(..), Constraints(..), landC)
import Syntax.Version (Version(Root))
import Translation.Promote
import Translation.Normalize (normalize)
import Translation.RenameExVars (renameExVarModule, duplicateEnvs, CounterTable)

type BundledTEnv = Map String TEnv
type BundledUEnv = Map String UEnv
type BundledConstraints = Map String Constraints


data CompileEnv' = CompileEnv'
  { pending :: [VLMod]  -- コンパイル待ちのモジュール、先頭からコンパイルされる
  , counter :: Int      -- 型変数生成用の通し番号
  , counterTable :: CounterTable -- 複製された外部モジュール変数の登場回数の通し番号
  , mapParsedAST :: Map VLMod (Module SrcSpanInfo) -- パース済みのAST
  , globalTEnv :: Map VLMod [TypedExp] -- 型検査後の型情報をそのまま格納するグローバル環境
  , globalConstraints :: Constraints -- 全ての制約が入っているグローバル制約環境
  , bundledTEnv :: BundledTEnv -- バンドル後の型情報が格納されている型環境
  , bundledUEnv :: BundledUEnv -- バンドル後の型変数情報が格納されている環境
  , bundledConstraints :: BundledConstraints -- bundledTEnvに対応する制約が格納されている環境
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

addGlobalTEnv :: VLMod -> [TypedExp] -> CompileEnv ()
addGlobalTEnv vlmod tyexp = state $ \env -> ((), env { globalTEnv = insert vlmod tyexp $ globalTEnv env })

addGlobalConstraints :: Constraints -> CompileEnv ()
addGlobalConstraints cs = modify $ \env -> env { globalConstraints = cs `landC` globalConstraints env }

addBundledTEnv :: String -> TEnv -> CompileEnv ()
addBundledTEnv s tenv = state $ \env -> ((), env { bundledTEnv = insert s tenv $ bundledTEnv env })

addBundledUEnv :: String -> UEnv -> CompileEnv ()
addBundledUEnv s uenv = state $ \env -> ((), env { bundledUEnv = insert s uenv $ bundledUEnv env })

addModuleConstraints :: String -> Constraints -> CompileEnv ()
addModuleConstraints s c = state $ \env -> ((), env { bundledConstraints = insert s c $ bundledConstraints env })

genEnvFromImports :: [String] -> CompileEnv (TEnv, UEnv, Constraints)
genEnvFromImports modules = do
  envs <- genEnvFromImports' modules
  let (newTEnv, newUEnv, newCons) = Data.List.foldl
                            (\(accTEnv, accUEnv, accC) (tenv,uenv,c) ->
                              ( accTEnv .++. tenv
                              , accUEnv `Data.Map.union` uenv
                              , accC `landC` c))
                            ( emptyEnv :: TEnv
                            , emptyEnv :: UEnv
                            , CTop)
                            envs
  return (newTEnv, newUEnv, newCons)
  where
    genEnvFromImports' :: [String] -> CompileEnv [(TEnv, UEnv, Constraints)]
    genEnvFromImports' [] = return []
    genEnvFromImports' (mn:lst) = do
      tenv' <- gets ((! mn) . bundledTEnv)
      uenv' <- gets ((! mn) . bundledUEnv)
      cs <- gets ((! mn) . bundledConstraints)
      xs <- genEnvFromImports' lst
      return $ (tenv', uenv', cs) : xs

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

  -- logE "\n=== Exactprint ==="
  -- logE $ exactPrint ast []

  logP "\n=== AST (Syntax.Absyn) ==="
  -- lift $ print ast
  -- logE ast
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
  let
    astVL =
      -- [TODO]
      -- past
      -- if v == Root -- Rootモジュールの場合はK-正規形の最終letのbodyをpromoteする
      --   then promoteMain $ girardFwd ast_normalized
      --   else girardFwd ast_normalized
      -- new
      -- 代わりに全モジュールの最終値をpromoteし、bundle時に追加された型変数にバージョン制約をのせる
      promoteTopVal $ girardFwd ast_normalized
  logP astVL

  logP "\n=== AST (Syntax.VL), external variables duplicated ==="
  let (astVLDuplicated, ct) = renameExVarModule astVL
  logP astVLDuplicated

  logP "\n=== Types (Syntax.VL) ==="
  (importedTEnv, importedUEnv, initConstraints) <- genEnvFromImports importMods
  initCounter <- gets counter
  logPD $ ppP "[DEBUG] Imported TEnv       :" <+> ppP importedTEnv    -- import宣言から得られた型環境
  logPD $ ppP "[DEBUG] Imported UEnv       :" <+> ppP importedUEnv    -- import宣言から得られた型変数環境
  logPD $ ppP "[DEBUG] Initial constraints :" <+> ppP initConstraints -- import宣言から得られたラベル制約
  logPD $ ppP "[DEBUG] Initial counter     :" <+> ppP initCounter     -- 型変数生成用の通し番号
  logPD line
  let (typedExpWithLogs, c) = getInterface importedTEnv importedUEnv initCounter astVL -- 型推論

  ---------------
  let (tenvDuplicated, uenvDuplicated, csDuplicated) = duplicateEnvs ct (importedTEnv, importedUEnv, initConstraints)
  logPD $ ppP "[DEBUG] Duplicated TEnv        :" <+> ppP tenvDuplicated
  logPD $ ppP "[DEBUG] Duplicated UEnv        :" <+> ppP uenvDuplicated
  logPD $ ppP "[DEBUG] Duplicated Constraints :" <+> ppP csDuplicated
  ----
  -- let (typedExpWithLogs, c) = getInterface tenvDuplicated uenvDuplicated initCounter astVL -- 型推論
  -- 
  forM_ typedExpWithLogs logP
  let typedExp = Data.List.map fst typedExpWithLogs
  addGlobalTEnv target typedExp
  setCounter c

  tempCounter <- gets counter
  logPD $ ppP "[DEBUG] counter:" <+> ppP tempCounter

  isLastVersion target >>= \b -> when (b && v /= Root) $ do
    logP "\n============================================"
    logPD $ ppP "=== Bundling top-level symbols of module" <+> ppP mn
    logP "============================================"
    c <- gets counter
    resultMap <- gets (Data.Map.filterWithKey (\(VLMod mn' _) _ -> mn' == mn) . globalTEnv)
    let (newTEnv, newUEnv, newCons, newCounter) = bundle mn c resultMap

    addBundledTEnv mn newTEnv
    logPD $ ppP "[DEBUG] Add tenv        :" <+> ppP newTEnv
    addBundledUEnv mn newUEnv
    logPD $ ppP "[DEBUG] Add uenv        :" <+> ppP newUEnv
    addModuleConstraints mn newCons
    logPD $ ppP "[DEBUG] Add constraints :" <+> ppP newCons
    setCounter newCounter
    logPD $ ppP "[DEBUG] New counter     :" <+> ppP newCounter

  bundledtenv <- gets bundledTEnv
  pendingMods <- gets pending
  logPD $ ppP "[DEBUG] bundledTEnv:" <+> ppP bundledtenv
  logPD $ ppP "[DEBUG] List of modules not yet compiled:"
  logP pendingMods
  logPD line

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