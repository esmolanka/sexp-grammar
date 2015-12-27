{-# LANGUAGE TemplateHaskell #-}
module Data.InvertibleGrammar.TH where

import Language.Haskell.TH
import Data.StackPrism.ReverseTH
import Data.InvertibleGrammar

grammarFor :: Name -> ExpQ
grammarFor name = [e| FPrism $(deriveRevStackPrism name) |]
