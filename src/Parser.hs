module Parser (
  parseAST
) where

import Language.Haskell.Exts hiding (parse)

parseAST :: FilePath -> IO (Module SrcSpanInfo)
parseAST path = fromParseResult <$> parseFile path