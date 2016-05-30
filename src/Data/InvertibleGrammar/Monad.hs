{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Data.InvertibleGrammar.Monad
  ( module Control.Monad.ContextError
  , dive
  , step
  , grammarError
  , runGrammarMonad
  , Propagation
  , GrammarError (..)
  , Mismatch (..)
  ) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Semigroup
import Control.Monad.ContextError

initPropagation :: Propagation
initPropagation = Propagation [0]

newtype Propagation = Propagation [Int]
  deriving (Eq, Show)

instance Ord Propagation where
  compare (Propagation as) (Propagation bs) =
    reverse as `compare` reverse bs

data Mismatch = Mismatch
  { mismatchExpected :: Set String
  , mismatchGot :: Maybe String
  } deriving (Show, Eq)

runGrammarMonad :: ContextError Propagation GrammarError a -> Either String a
runGrammarMonad m =
  case runContextError m initPropagation of
    Left (GrammarError _ mismatch) -> Left $ renderMismatch mismatch
    Right a -> Right a

renderMismatch :: Mismatch -> String
renderMismatch (Mismatch (S.toList -> expected) got) =
  case (expected, got) of
    ([], Nothing) -> "Unexpected error happened"
    ([], Just got') -> "Got unexpected " ++ got'
    (_:_, Nothing) -> "Expected " ++ intercalate ", " expected
    (_:_, Just got') -> "Expected " ++ intercalate ", " expected ++
                        "\nGot unexpected " ++ got'
data GrammarError = GrammarError Propagation Mismatch
  deriving (Show, Eq)

instance Semigroup GrammarError where
  GrammarError pos m <> GrammarError pos' m'
    | pos > pos' = GrammarError pos m
    | pos < pos' = GrammarError pos' m'
    | otherwise  = GrammarError pos $
      Mismatch
        (mismatchExpected m <> mismatchExpected m')
        (mismatchGot m <|> mismatchGot m')

dive :: MonadContextError Propagation e m => m a -> m a
dive =
  localContext $ \(Propagation xs) ->
    Propagation (0 : xs)

step :: MonadContextError Propagation e m => m ()
step =
  modifyContext $ \(Propagation xs) ->
    Propagation $ case xs of
      (x : xs) -> succ x : xs
      [] -> [0]

grammarError :: MonadContextError Propagation GrammarError m => Mismatch -> m a
grammarError mismatch = throwInContext (\ctx -> GrammarError ctx mismatch)
