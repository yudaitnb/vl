module Util (
  pp, logPpLn,
  Pretty,
  removeFileIfExists, createDirectoryIfMissing
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

-- logStrLn :: Show a => FilePath -> a -> IO ()
-- logStrLn filename str = do
--   print str
--   appendFile filename $ show str ++ "\n"

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path