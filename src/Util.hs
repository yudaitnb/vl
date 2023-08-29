{-# LANGUAGE DefaultSignatures #-}
module Util (
  Extension(..), mkPath,
  logPpLn, logPpLnDoc,
  module Prettyprinter,
  module Prettyprinter.Render.Text,
  removeFileIfExists, createDirectoryIfMissing,
  PrettyAST(..),
  putDocString,
  vdash, semicolon, emptyset, pplist,
  Language.Haskell.Exts.ExactPrint.exactPrint,
  (<!>),
) where

import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )

import Language.Haskell.Exts.ExactPrint ( exactPrint )
import Prettyprinter.Render.Text
import Prettyprinter hiding (prettyList)
import Prettyprinter.Render.String ( renderString )
import Control.Monad (when)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Extension = String

mkPath :: FilePath -> FilePath -> Extension -> FilePath
mkPath p mn ext = p ++ mn ++ ext

--------------

logPpLn :: (PrettyAST a) => (a -> Doc ann) -> FilePath -> a -> IO ()
logPpLn pp logFilePath ast = do
  let doc = pp ast <> line
  putDoc doc
  appendFile logFilePath (renderString $ layoutPretty defaultLayoutOptions doc)

logPpLnDoc :: FilePath -> Doc ann -> IO ()
logPpLnDoc logFilePath ast = do
  let doc = ast <> line
  putDoc doc
  appendFile logFilePath (renderString $ layoutPretty defaultLayoutOptions doc)

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

(<!>) :: (Show k, Show a, Ord k) => Map k a -> k -> a
m <!> k = fromMaybe
            (error $
              "given key is not an element in the map." ++ 
              "\n  key : " ++ show k ++ 
              "\n  map : " ++ show m ++ "\n") $
            M.lookup k m

------------------------

class PrettyAST a where
  ppP :: a -> Doc ann
  default ppP :: Show a => a -> Doc ann
  ppP = viaShow

instance PrettyAST Char where
  ppP = pretty

instance PrettyAST String where
  ppP = pretty

instance PrettyAST Integer where
  ppP = pretty

instance PrettyAST Int where
  ppP = pretty

instance PrettyAST Float where
  ppP = pretty

instance PrettyAST Double where
  ppP = pretty

instance PrettyAST Bool where
  ppP = pretty

instance (PrettyAST a, PrettyAST b) => PrettyAST (a, b) where
  ppP (a,b) = vsep [ppP a, ppP b] 

instance PrettyAST a => PrettyAST (Maybe a) where
  ppP = maybe mempty ppP

pplist :: (a -> Doc ann) -> [a] -> Doc ann
pplist pp = align . list . map pp
