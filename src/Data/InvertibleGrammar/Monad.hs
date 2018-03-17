{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.InvertibleGrammar.Monad
  ( module Control.Monad.ContextError
  , dive
  , step
  , locate
  , grammarError
  , runGrammarMonad
  , Propagation
  , GrammarError (..)
  , Mismatch
  , expected
  , unexpected
  ) where

import Control.Applicative
import Control.Monad.ContextError

import Data.Semigroup as Semi
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

import Data.Text.Prettyprint.Doc
  (Pretty, pretty, vsep, indent, fillSep, punctuate, comma, (<+>))

initPropagation :: p -> Propagation p
initPropagation = Propagation [0]

data Propagation p = Propagation
  { pProp :: [Int]
  , pPos  :: p
  } deriving (Show)

instance Eq (Propagation p) where
  Propagation xs _ == Propagation ys _ = xs == ys
  {-# INLINE (==) #-}

instance Ord (Propagation p) where
  compare (Propagation as _) (Propagation bs _) =
    reverse as `compare` reverse bs
  {-# INLINE compare #-}

-- | Data type to encode mismatches during parsing or generation, kept abstract.
-- It is suggested to use 'expected' and 'unexpected' constructors to build a
-- mismatch report.
data Mismatch = Mismatch
  { mismatchExpected :: Set Text
  , mismatchGot :: Maybe Text
  } deriving (Show, Eq)

-- | Construct a mismatch report with specified expectation. Can be appended
-- to other expectations and 'unexpected' reports to clarify a mismatch.
expected :: Text -> Mismatch
expected a = Mismatch (S.singleton a) Nothing

-- | Construct a mismatch report with information what has been occurred during
-- processing but is not expected.
unexpected :: Text -> Mismatch
unexpected a = Mismatch S.empty (Just a)

instance Semigroup Mismatch where
  m <> m' =
    Mismatch
      (mismatchExpected m Semi.<> mismatchExpected m')
      (mismatchGot m <|> mismatchGot m')
  {-# INLINE (<>) #-}

instance Monoid Mismatch where
  mempty = Mismatch mempty mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

runGrammarMonad :: p -> (p -> String) -> ContextError (Propagation p) (GrammarError p) a -> Either String a
runGrammarMonad initPos showPos m =
  case runContextError m (initPropagation initPos) of
    Left (GrammarError p mismatch) ->
      Left $ renderMismatch (showPos (pPos p)) mismatch
    Right a -> Right a

instance Pretty Mismatch where
  pretty (Mismatch (S.toList -> []) Nothing) =
    "unknown mismatch occurred"
  pretty (Mismatch (S.toList -> expected) got) =
    vsep [ ppExpected expected
         , ppGot got
         ]
    where
      ppExpected []  = mempty
      ppExpected xs  = "expected:" <+> fillSep (punctuate comma $ map pretty xs)
      ppGot Nothing  = mempty
      ppGot (Just a) = "     got:" <+> pretty a

renderMismatch :: String -> Mismatch -> String
renderMismatch pos mismatch =
  show $ vsep
    [ pretty pos `mappend` ":" <+> "mismatch:"
    , indent 2 $ pretty mismatch
    ]

data GrammarError p = GrammarError (Propagation p) Mismatch
  deriving (Show)

instance Semigroup (GrammarError p) where
  GrammarError pos m <> GrammarError pos' m'
    | pos > pos' = GrammarError pos m
    | pos < pos' = GrammarError pos' m'
    | otherwise  = GrammarError pos (m <> m')
  {-# INLINE (<>) #-}

dive :: MonadContextError (Propagation p) e m => m a -> m a
dive =
  localContext $ \(Propagation xs pos) ->
    Propagation (0 : xs) pos
{-# INLINE dive #-}

step :: MonadContextError (Propagation p) e m => m ()
step =
  modifyContext $ \propagation ->
    propagation
      { pProp = case pProp propagation of
          (x : xs) -> succ x : xs
          [] -> [0]
      }
{-# INLINE step #-}

locate :: MonadContextError (Propagation p) e m => p -> m ()
locate pos =
  modifyContext $ \propagation ->
    propagation { pPos = pos }
{-# INLINE locate #-}

grammarError :: MonadContextError (Propagation p) (GrammarError p) m => Mismatch -> m a
grammarError mismatch =
  throwInContext $ \ctx ->
    GrammarError ctx mismatch
{-# INLINE grammarError #-}
