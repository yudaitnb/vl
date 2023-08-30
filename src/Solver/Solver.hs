module Solver.Solver (
  solve
) where

import Prelude hiding (log)

import Data.Map ( Map, empty, mapWithKey, toList )

import Syntax.Common
import Syntax.Type (Constraints)

import Parser

import Solver.Engine.SolverZ3
import Util
-- import Solver.Engine.SolverGraph (solveCs)

solve :: Map String [Version] -> FilePath -> Constraints -> IO SRSuccess
solve m logFilePath cs = do
  logP "=== Solver result ==="
  solveCs m cs >>= \case
    Left (h, r) -> do
      logP h
      logPD $ concatWith (surround $ comma <> space) $ map ppP r
      error "Whole process finished."

    Right res -> do
      logPD $ concatWith (surround line) $ mapWithKey (\vn l -> ppP vn <+> colon <+> ppP l) res
      return res

  where
    logP :: PrettyAST a => a -> IO ()
    logP = logPpLn ppP logFilePath
    logPD :: Doc ann -> IO ()
    logPD = logPpLnDoc logFilePath