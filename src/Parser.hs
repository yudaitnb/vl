module Parser (parseAST) where

-- from haskell-src-exts
import Language.Haskell.Exts
import qualified Language.Haskell.Exts.Syntax as S
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Extension as E
import qualified Language.Haskell.Exts.SrcLoc as L

parseAST :: FilePath -> IO (Module SrcSpanInfo)
parseAST path = fromParseResult <$> parseFile path