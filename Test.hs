
import System.Environment

import qualified Data.Text.Lazy.IO as T

import Language.Sexp.Types
import Language.Sexp.Lexer
import Language.Sexp.Parser
import Language.Sexp.Pretty

parseSexpsFrom :: FilePath -> String -> Either String [Sexp]
parseSexpsFrom fn = parseProgram . lexSexp fn

main :: IO ()
main = do
  args <- getArgs
  msexps <-
    case args of
      []     -> parseSexpsFrom "<stdin>" <$> getContents
      (fn:_) -> parseSexpsFrom fn <$> readFile fn

  case msexps of
    Left err -> putStrLn err
    Right sexps -> mapM_ (T.putStrLn . printSexp) sexps
