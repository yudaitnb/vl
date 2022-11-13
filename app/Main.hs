module Main where

import Prelude hiding (log)
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( splitFileName, splitExtension )
import System.Environment ( getArgs )

import Control.Monad ( forM_, when )
import Control.Monad.State ( execStateT, execState )

import Data.Map ( Map, (!), empty, elems, mapWithKey, fromList, toList )
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)

import Parser ( cvtExtMods, parseDependencyGraph, VLMod(..) )
import Compile (compile, CompileEnv'(..))
import Inference.TypeInference (TypedExp(..), aggregateConstraints)
import Solver ( solve, combineSolRes )
import Config ( decodeConfig )
import Translation.Extraction ( extract )
import Translation.Girard ( girardBck )

import Syntax.Type (landC, Constraints(..), Type, HasName (getName))
import Syntax.Common (Version(..), Label, ParsedAST(..))

-- import Language.Haskell.Names as HN
import Language.Haskell.Exts.Pretty as PP

import Util

-- import qualified Language.Haskell.Interpreter as I

main :: IO ()
main = do
  -- Configuring as vlconfig.json
  (rootDirPath, logDirPath, validExt) <- decodeConfig

  -- Get a filename of a root module from stdin, e.g. `stack run "[ROOT_VL_MODULE].hs"`
  [fnRoot] <- getArgs
  let (fnMain, ext) = splitExtension fnRoot
  when (ext /= validExt) $ error "Illegal VL module extension."

  -- Logger utility
  let logFilePath = logDirPath ++ fnMain ++ ".log"
      logP :: PrettyAST a => a -> IO ()
      logP = logPpLn ppP logFilePath
      logPD :: Doc ann -> IO ()
      logPD = logPpLnDoc logFilePath

  -- Create log directory 
  createDirectoryIfMissing False logDirPath
  removeFileIfExists logFilePath

  -- Parse dependent modules and create dependency graph
  logP "=== Parsing ==="
  logPD $ ppP "[DEBUG] rootDirPath :" <+> ppP rootDirPath
  logPD $ ppP "[DEBUG] logDirPath  :" <+> ppP logDirPath
  logPD $ ppP "[DEBUG] validExt    :" <+> ppP validExt
  (root, sortedVLMods, mapParsedAst) <- parseDependencyGraph fnMain rootDirPath ext logFilePath
  
  logP "=== Parsed Modules ==="
  logPD $ ppP mapParsedAst
  logP . show $ mapParsedAst <!> VLMod "Main" Root

  logP "=== Compilation order ==="
  logP sortedVLMods
  env <- compile sortedVLMods mapParsedAst logFilePath

  logP "=== External Variables ==="
  let exVarsRes = exVarResources env
  logPD $ concatWith (surround line) $ mapWithKey (\k (orig, m, ty) -> ppP k <+> ppP m <+> ppP ty) exVarsRes

  logP "\n=== Constraints ==="
  let cons = globalConstraints env
  logPD $ ppP cons <> line

  logP "=== Solver result ==="
  solResMap <- fromMaybe empty <$> (Solver.solve (cvtExtMods sortedVLMods) cons >>= \case
    Left (h, r) -> do
      logP h
      logPD $ concatWith (surround $ comma <> space) $ map ppP r
      return Nothing
    Right res -> do
      logPD $ concatWith (surround line) $ mapWithKey (\vn l -> ppP vn <+> colon <+> ppP l) res
      return $ Just res)

  logP "\n=== Labels of External Variables ==="
  let exVarLabels = combineSolRes exVarsRes solResMap
  logPD $ ppP exVarLabels

  logP "\n=== Prettyprint ==="
  forM_ (toList $ mapParsedAST env) $ \(VLMod a v, ast) -> do
    logPD $ ppP "\n--" <+> ppP a <+> ppP "v" <> ppP v
    logP $ prettyPrint ast

  logP "\n=== Extraction ==="
  let vldecls = mapVLDecls env
      expMain = vldecls <!> VLMod "Main" Root <!> "main"
      extracted = extract exVarLabels vldecls expMain
      hsCodeBundled = girardBck extracted
  logP vldecls
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
