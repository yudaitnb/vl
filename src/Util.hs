{-# LANGUAGE FlexibleInstances #-}
module Util (
  pp, logPpLn, logPpASTLn,
  module Prettyprinter,
  removeFileIfExists, createDirectoryIfMissing,
  PrettyAST(..),
  putDocString,
  vdash, semicolon, emptyset
) where

import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )

import Prettyprinter.Render.Text
import Prettyprinter
import Prettyprinter.Render.String ( renderString )
import Control.Monad (when)

pp :: Pretty a => a -> IO ()
pp ast = putDoc $ pretty ast <+> line

logPpLn :: Pretty a => FilePath -> a -> IO ()
logPpLn filename ast = do
  let doc = pretty ast <> line
  putDoc doc
  appendFile filename (renderString $ layoutPretty defaultLayoutOptions doc)

logPpASTLn :: PrettyAST a => FilePath -> a -> IO ()
logPpASTLn filename ast = do
  let doc = ppP ast <> line
  putDoc doc
  appendFile filename (renderString $ layoutPretty defaultLayoutOptions doc)

-- logStrLn :: Show a => FilePath -> a -> IO ()
-- logStrLn filename str = do
--   print str
--   appendFile filename $ show str ++ "\n"

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

putDocString :: Doc ann -> String
putDocString = renderString . layoutCompact

semicolon :: Doc ann
semicolon = pretty ";"

vdash :: Doc ann
vdash = line <> pretty "|-" <> line

emptyset :: Doc ann
emptyset = pretty "{}"

------------------------

class (Pretty a) => PrettyAST a where
  ppE :: a -> Doc ann
  ppP :: a -> Doc ann

instance PrettyAST String where
  ppE = pretty
  ppP = pretty

instance (PrettyAST a, PrettyAST b) => PrettyAST (a, b) where
  ppE (a,b) = vsep [ppE a, ppE b] 
  ppP (a,b) = vsep [ppP a, ppP b] 