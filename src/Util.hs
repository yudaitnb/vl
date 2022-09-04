{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures   #-}
module Util (
  pp, logPpLn,
  module Prettyprinter,
  removeFileIfExists, createDirectoryIfMissing,
  PrettyAST(..),
  putDocString,
  vdash, semicolon, emptyset, pplist
) where

import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )

import Prettyprinter.Render.Text
import Prettyprinter hiding (prettyList)
import Prettyprinter.Render.String ( renderString )
import Control.Monad (when)

pp :: PrettyAST a => a -> IO ()
pp ast = putDoc $ ppE ast <+> line

logPpLn :: (PrettyAST a) => (a -> Doc ann) -> FilePath -> a -> IO ()
logPpLn pp filename ast = do
  let doc = pp ast <> line
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

------------------------

putDocString :: Doc ann -> String
putDocString = renderString . layoutCompact

semicolon :: Doc ann
semicolon = pretty ";"

vdash :: Doc ann
vdash = 
  space <> pretty "|-" <> space
  -- space <> pretty "|-" <> line

emptyset :: Doc ann
emptyset = pretty "{}"

------------------------

class PrettyAST a where
  ppE :: a -> Doc ann
  ppP :: a -> Doc ann
  default ppE :: Show a => a -> Doc ann
  default ppP :: Show a => a -> Doc ann
  ppE = viaShow
  ppP = viaShow

instance PrettyAST Char where
  ppE = pretty
  ppP = pretty

instance PrettyAST String where
  ppE = pretty
  ppP = pretty

instance PrettyAST Integer where
  ppE = pretty
  ppP = pretty

instance PrettyAST Int where
  ppE = pretty
  ppP = pretty

instance PrettyAST Bool where
  ppE = pretty
  ppP = pretty

instance (PrettyAST a, PrettyAST b) => PrettyAST (a, b) where
  ppE (a,b) = vsep [ppE a, ppE b] 
  ppP (a,b) = vsep [ppP a, ppP b] 

instance PrettyAST a => PrettyAST (Maybe a) where
  ppE = maybe mempty ppE
  ppP = maybe mempty ppP

pplist :: (a -> Doc ann) -> [a] -> Doc ann
pplist pp = align . list . map pp