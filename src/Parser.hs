module Parser (
  parseAST,
  parseString
) where

import Language.Haskell.Exts hiding (parse)

parseAST :: FilePath -> IO (Module SrcSpanInfo)
parseAST path = fromParseResult <$> parseFile path

parseString :: String -> Module SrcSpanInfo
parseString ss = fromParseResult $ parseFileContents ss