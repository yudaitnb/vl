module Main where

import System.FilePath ( splitFileName )
import System.Environment ( getArgs )

import Parser ( parseAST )
import Language.Haskell.Exts.ExactPrint ( exactPrint )
-- import Syntax.LambdaVL
import Syntax.Absyn
import Syntax.SrcLoc
import Renamer
import Girard ( girardFwd )
import Inference.TypeInference
import Util
import Desugar
import Control.Monad (forM_)

-- from hint
-- import qualified Language.Haskell.Interpreter as H
-- import Language.Haskell.Interpreter.Unsafe

main :: IO ()
main = do
  [path, func] <- getArgs
  let (dir, fn) = splitFileName path
      logdir = "./log/"
      logfile = logdir ++ fn ++ ".log"
      logE :: PrettyAST a => a -> IO ()
      logE = logPpLn ppE logfile
      logP :: PrettyAST a => a -> IO ()
      logP = logPpLn ppP logfile

  -- create log directory 
  createDirectoryIfMissing False logdir
  removeFileIfExists logfile

  -- output
  ast <- parseAST path
  logE "\n=== Exactprint ==="
  logE $ exactPrint ast []
  logE "\n=== AST (Syntax.Absyn) ==="
  logE ast
  -- log "=== Alpha renaming ==="
  -- let ast_renamed = alphaRename ast
  -- log ast_renamed
  logP "\n=== Desugared AST (Syntax.STLC) ==="
  let ast_desugared = desugarAST ast
  logP ast_desugared
  logP "\n=== AST (Syntax.VL) ==="
  -- let ast_vl = girardFwd ast_renamed
  let ast_vl = girardFwd ast_desugared
  logP ast_vl
  logE ast_vl
  logP "\n=== Types (Syntax.VL) ==="
  let lst = getInterface ast_vl
  forM_ lst logP

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