module Main where

import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( splitFileName, splitExtension )
import System.Environment ( getArgs )

import Control.Monad ( forM_, when )
import Control.Monad.State ( execStateT, execState )

import Data.List (nub)
import Data.Map ( Map, (!), empty, elems, mapWithKey, fromList, toList )
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Either (fromRight)

import DependencyGraph
import Compile
import Inference.TypeInference (TypedExp(..), aggregateConstraints)
import Solver
import SolverGT
import Translation.Extraction
import Translation.Girard

import Syntax.Type (landC, Constraints(..), Type, HasName (getName))
import qualified Data.List
import Syntax.Version (Version(..))
import Syntax.Label (Label)
import Syntax.Absyn hiding (Type(..))
import Syntax.Name

-- import Language.Haskell.Names as HN
import Language.Haskell.Exts.Pretty as PP

import Util

-- import qualified Language.Haskell.Interpreter as I

main :: IO ()
main = do
  let rootDirPath = "./examples/"
      logDirPath = "./log/"
      extension = ".hs"

  -- Get arguments from stdin
  -- stack run "[ROOT].hs"
  [filename] <- getArgs
  let dir = rootDirPath
      (fn, ext) = splitExtension filename

  -- Logger utility
  let logfile = logDirPath ++ fn ++ ".log"
      logE :: PrettyAST a => a -> IO ()
      logE = logPpLn ppE logfile
      logP :: PrettyAST a => a -> IO ()
      logP = logPpLn ppP logfile

  -- Create log directory 
  createDirectoryIfMissing False logDirPath
  removeFileIfExists logfile

  -- Parse dependent modules and create dependency graph
  (root, sorted, mapParsedAst) <- getDependencyGraph fn dir ext
  let extMods = cvtExtMods sorted
  logP "=== Compilation order ==="
  logP sorted

  let initCompEnv = CompileEnv' (tail sorted) 0 empty mapParsedAst empty empty CTop empty empty empty empty empty logfile
  env <- execStateT (compile $ head sorted) initCompEnv

  logP "=== External Variables ==="
  let exVarsRes = exVarResources env
  logP $ putDocString $ concatWith (surround line) $ mapWithKey (\k (orig, m, ty) -> ppP k <+> ppP m <+> ppP ty) exVarsRes

  logP "\n=== Constraints ==="
  let cons = globalConstraints env
  logP $ putDocString $ ppP cons <> line

  logP "=== Solver result ==="
  -- let fvCons = freeVars cons
  --     maxWHeader = maximum $ Data.List.map Data.List.length fvCons
  --     maxWItem = 7 -- [TODO]
  --     printResult :: Int -> Int -> [(String, [(String, Version)])] -> IO ()
  --     printResult maxWHeader maxWItem m = do
  --       forM_ m $ \(v, res) -> do
  --         let header = fill maxWHeader (ppP v) <+> ppP ":"
  --             maintxt = concatWith (surround $ comma <> space) $ Data.List.map (\(mn, v) -> fill maxWItem $ ppP mn <> colon <> ppP v) res
  --             doc = header <+> maintxt
  --         logP $ putDocString doc

  solResMap <- fromMaybe empty <$> (Solver.solve extMods cons >>= \case
    Left (h,r) -> do
      logP h
      logP $ putDocString $ concatWith (surround $ comma <> space) $ map ppP r
      return Nothing
    Right res -> do
      logP $ putDocString $ concatWith (surround line) $ mapWithKey (\vn l -> ppP vn <+> colon <+> ppP l) res
      return $ Just res)

  logP "\n=== Labels of External Variables ==="
  let exVarLabels = combineSolRes exVarsRes solResMap
  logP $ putDocString $ ppP exVarLabels

  -- let astAv100 = mapParsedAST env ! VLMod "A" (Version 1 0 0)
  --     astBv100 = mapParsedAST env ! VLMod "B" (Version 1 0 0)
  -- let a = HN.annotate (fromList
  --         [ ( ModuleName () "B"
  --           , [ Value (ModuleName () "B") (Ident () "f")
  --             , Value (ModuleName () "B") (Ident () "y")
  --             ]
  --           )
  --         ]
  --       ) astAv100
  -- print a

  logP "\n=== Prettyprint ==="
  forM_ (toList $ mapParsedAST env) $ \(VLMod a v, ast) -> do
    logP $ putDocString $ ppP "\n--" <+> ppP a <+> ppP "v" <> ppP v
    logP $ prettyPrint ast

  logP "\n=== Extraction ==="
  let vldecls = mapVLDecls env
      expMain = vldecls ! "main"
      extracted = extract exVarLabels vldecls expMain
      hsCodeBundled = girardBck extracted
  logP $ prettyPrint hsCodeBundled

  -- putStrLn "=== Standard output ==="
  -- res <- H.runInterpreter $ interp tmpfn func
  -- putStrLn "=== Result value ( expects () ) ==="
  -- case res of
  --   Right res -> print res
  --   Left err  -> throw err

-- interp :: String -> String -> H.InterpreterT IO ()
-- interp path func = do
--   initInterpEnvByFile path
--   H.runStmt func

-- initInterpEnvByFile :: String -> H.Interpreter ()
-- initInterpEnvByFile path = do
--   let mod = dropExtension $ takeFileName path
--   H.reset
--   -- unsafeSetGhcOption ("-i" ++ intercalate ":" confs)
--   -- H.set [H.languageExtensions H.:= [H.ImplicitParams]]
--   -- H.set [H.installedModulesInScope H.:= True]
--   H.loadModules [path]
--   H.setTopLevelModules [mod]
--   H.setImportsQ [("Prelude", Nothing)]

-- A -> [ (f -> "a?")]
combineSolRes :: Map String (String, VLMod, Type) -> Map String Label -> Map String (String, VLMod, Type, Label)
combineSolRes evr solres = mapWithKey (\vn (s, vlmod,ty) -> (s, vlmod, ty, solres ! getName ty)) evr

instance PrettyAST (Map String (String, VLMod, Type, Label)) where
  ppE m = concatWith (surround line) $ mapWithKey (\vn (s, m, tv, l) -> ppE vn <+> colon <+> ppE s <+> colon <+> ppE m <+> colon <+> ppE tv <+> colon <+> ppE l) m
  ppP m = concatWith (surround line) $ mapWithKey (\vn (s, m, tv, l) -> ppP vn <+> colon <+> ppP s <+> colon <+> ppP m <+> colon <+> ppP tv <+> colon <+> ppP l) m