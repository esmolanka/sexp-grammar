{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Data.InvertibleGrammar.Monad
  ( module Control.Monad.ContextError
  , dive
  , step
  , locate
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

initPropagation :: p -> Propagation p
initPropagation = Propagation [0]

data Propagation p = Propagation
  { pProp :: [Int]
  , pPos  :: p
  } deriving (Show)

instance Eq (Propagation p) where
  Propagation xs _ == Propagation ys _ = xs == ys

instance Ord (Propagation p) where
  compare (Propagation as _) (Propagation bs _) =
    reverse as `compare` reverse bs

data Mismatch = Mismatch
  { mismatchExpected :: Set String
  , mismatchGot :: Maybe String
  } deriving (Show, Eq)

runGrammarMonad :: p -> (p -> String) -> ContextError (Propagation p) (GrammarError p) a -> Either String a
runGrammarMonad initPos showPos m =
  case runContextError m (initPropagation initPos) of
    Left (GrammarError p mismatch) ->
      Left $ renderMismatch (showPos (pPos p)) mismatch
    Right a -> Right a

renderMismatch :: String -> Mismatch -> String
renderMismatch pos (Mismatch (S.toList -> expected) got) =
  pos ++ ": " ++
    case (expected, got) of
      ([], Nothing) -> "Unexpected error happened"
      ([], Just got') -> "Got unexpected " ++ got'
      (_:_, Nothing) -> "Expected " ++ intercalate ", " expected
      (_:_, Just got') -> "Expected " ++ intercalate ", " expected ++
                          "\nGot unexpected " ++ got'

data GrammarError p = GrammarError (Propagation p) Mismatch
  deriving (Show)

instance Semigroup (GrammarError p) where
  GrammarError pos m <> GrammarError pos' m'
    | pos > pos' = GrammarError pos m
    | pos < pos' = GrammarError pos' m'
    | otherwise  = GrammarError pos $
      Mismatch
        (mismatchExpected m <> mismatchExpected m')
        (mismatchGot m <|> mismatchGot m')

dive :: MonadContextError (Propagation p) e m => m a -> m a
dive =
  localContext $ \(Propagation xs pos) ->
    Propagation (0 : xs) pos

step :: MonadContextError (Propagation p) e m => m ()
step =
  modifyContext $ \propagation ->
    propagation
      { pProp = case pProp propagation of
          (x : xs) -> succ x : xs
          [] -> [0]
      }

locate :: MonadContextError (Propagation p) e m => p -> m ()
locate pos =
  modifyContext $ \propagation ->
    propagation { pPos = pos }


grammarError :: MonadContextError (Propagation p) (GrammarError p) m => Mismatch -> m a
grammarError mismatch = throwInContext (\ctx -> GrammarError ctx mismatch)
