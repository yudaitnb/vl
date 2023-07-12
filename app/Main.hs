module Main where

import Prelude hiding (log)
import System.Directory ( createDirectoryIfMissing, doesPathExist )
import System.FilePath ( splitFileName, splitExtension )
import System.Environment ( getArgs )

import Control.Monad ( forM_, when, unless )
import Control.Monad.State ( execStateT, execState )

import Data.Map ( Map, empty, mapWithKey, toList )
import Data.Maybe (fromMaybe, isJust)
import Data.Either (fromRight)

import Parser ( cvtExtMods, parseDependencyGraph, VLMod(..) )
import Compile (compile, CompileEnv'(..))
import Inference.TypeInference (TypedExp(..), aggregateConstraints)
import SolverZ3 ( solve, combineSolRes )
import Config ( decodeConfig )
import Translation.Extraction ( extract )
import Translation.Girard ( girardBck )

import Syntax.Type (landC, Constraints(..), Type, HasName (getName))
import Syntax.Common (Version(..), Label, ParsedAST(..))

import Language.Haskell.Exts.Pretty

import Evaluation
import Util

import Solver

-- import qualified Language.Haskell.Interpreter as I

main :: IO ()
main = do
  -- Configuring as vlconfig.json
  (rootDirPath, validExt) <- decodeConfig

  -- Get a filename of a root module from stdin, e.g. `stack run "[ROOT_VL_MODULE].hs"`
  [fnRoot] <- getArgs
  let (fnMain, ext) = splitExtension fnRoot
      pathMain = rootDirPath ++ fnMain ++ ext
  when (ext /= validExt) $ error "Illegal VL module extension."
  doesPathExist pathMain >>= \b -> unless b $ error $ show pathMain ++ " does not exists."

  -- Logger utility
  let logFilePath = rootDirPath ++ fnMain ++ ".log"
      logP :: PrettyAST a => a -> IO ()
      logP = logPpLn ppP logFilePath
      logPD :: Doc ann -> IO ()
      logPD = logPpLnDoc logFilePath
  removeFileIfExists logFilePath

  -- Parse dependent modules and create dependency graph
  logP "=== Parsing ==="
  logPD $ ppP "[DEBUG] rootDirPath :" <+> ppP rootDirPath
  logPD $ ppP "[DEBUG] validExt    :" <+> ppP validExt
  (timeParsing, (root, sortedVLMods, mapParsedAst)) <- timeItT $ parseDependencyGraph fnMain rootDirPath ext logFilePath
  
  logP "=== Parsed Modules ==="
  logP mapParsedAst

  logP "=== Compilation order ==="
  logP sortedVLMods
  (timeCompiling, env) <- timeItT $ compile sortedVLMods mapParsedAst logFilePath

  logP "=== External Variables ==="
  let exVarsRes = duplicatedExVars env
  logPD $ concatWith (surround line) $ mapWithKey (\k (orig, m, ty) -> ppP k <+> ppP m <+> ppP ty) exVarsRes

  logP "\n=== Constraints ==="
  let cons = globalConstraints env
  logPD $ ppP cons <> line

  logP "\n=== Constraints (normalized) ==="
  logPD $ ppP (mkOrdLstOfAndCs cons)


  logP "=== Solver result ==="
  (timeConstraintResolution, solResMap) <- timeItT $ SolverZ3.solve (cvtExtMods sortedVLMods) cons >>= \case
    Left (h, r) -> do
      logP h
      logPD $ concatWith (surround $ comma <> space) $ map ppP r
      return Nothing
    Right res -> do
      logPD $ concatWith (surround line) $ mapWithKey (\vn l -> ppP vn <+> colon <+> ppP l) res
      return $ Just res

  if isJust solResMap
    then do
      logP "\n=== Labels of External Variables ==="
      let exVarLabels = combineSolRes exVarsRes $ fromMaybe empty solResMap
      logPD $ ppP exVarLabels

      -- logP "\n=== Prettyprint ==="
      -- forM_ (toList $ mapParsedAST env) $ \(VLMod a v, ast) -> do
      --   logPD $ ppP "\n--" <+> ppP a <+> ppP "v" <> ppP v
      --   logP $ prettyPrint ast

      logP "\n=== VLDecls ==="
      let vldecls = mapVLDecls env
      logP vldecls

      logP "\n=== Extraction ==="
      let expMain = vldecls <!> VLMod "Main" Root <!> "main"
          extracted = extract exVarLabels vldecls expMain
          hsCodeBundled = girardBck extracted
      logP $ prettyPrint hsCodeBundled
    else
      logP "Whole process finished."

  logPD $ unlineTimes 
    [ ("Parsing", timeParsing)
    , ("Compiling", timeCompiling)
    , ("ConstraintResolution", timeConstraintResolution )]

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
