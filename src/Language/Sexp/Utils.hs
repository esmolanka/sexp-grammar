
module Language.Sexp.Utils
  ( lispifyName
  ) where

import Data.Char
import Data.List
import Data.List.Split

lispifyName :: String -> String
lispifyName =
  intercalate "-" .
    map (map toLower) .
    split (dropBlanks . dropInitBlank . condense . keepDelimsL $ whenElt isUpper)
