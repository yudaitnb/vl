module Main where

import System.FilePath ( splitFileName )
import System.Environment ( getArgs )
import System.Directory ( createDirectoryIfMissing )

import Parser ( parseAST )
import Language.Haskell.Exts.Pretty ( prettyPrint )
-- import VL
import Girard ( girardFwd )
import ASTPrinter ( pp )

-- from hint
-- import qualified Language.Haskell.Interpreter as H
-- import Language.Haskell.Interpreter.Unsafe

main :: IO ()
main = do
  [path, func] <- getArgs
  let (dir, fn) = splitFileName path
      tmpdir = dir ++ "tmp/"
      tmpfn = tmpdir ++ fn

  -- create temp directory 
  createDirectoryIfMissing False tmpdir

  -- output
  putStrLn "=== Parsing started ==="
  ast <- parseAST path
  putStrLn "=== AST ==="
  pp ast
  -- putStrLn "=== Prettyprint ==="
  -- let pretty = prettyPrint ast
  -- putStrLn pretty
  -- writeFile tmpfn pretty

  putStrLn "=== VL Module ==="
  let ast_vl = girardFwd ast
  pp ast_vl

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