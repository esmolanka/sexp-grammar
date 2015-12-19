
import System.Environment

import qualified Data.Text.Lazy.IO as T

import Language.Sexp

main :: IO ()
main = do
  args <- getArgs
  msexps <-
    case args of
      []     -> parseSexpsFrom "<stdin>" <$> getContents
      (fn:_) -> parseSexpsFrom fn <$> readFile fn

  case msexps of
    Left err -> putStrLn err
    Right sexps -> T.putStrLn (printSexps sexps)
