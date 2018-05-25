{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.InvertibleGrammar.Monad
  ( module Control.Monad.ContextError
  , runGrammar
  , runGrammarDoc
  , runGrammarString
  , ErrorMessage (..)
  , doAnnotate
  , doDive
  , doStep
  , doLocate
  , doError
  , Propagation
  , GrammarError (..)
  , Mismatch
  , expected
  , unexpected
  ) where

import Control.Arrow (left)
import Control.Applicative
import Control.Monad.ContextError

import Data.Maybe
import Data.Semigroup as Semi
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

import Data.Text.Prettyprint.Doc
  ( Doc, Pretty, pretty, vsep, hsep, line, indent, fillSep, punctuate
  , comma, colon, (<+>), layoutSmart, PageWidth(..), LayoutOptions(..)
  )

import Data.Text.Prettyprint.Doc.Render.String

initPropagation :: p -> Propagation p
initPropagation = Propagation [0] []

data Propagation p = Propagation
  { pProp :: [Int]
  , pAnns :: [Text]
  , pPos  :: p
  } deriving (Show)

instance Eq (Propagation p) where
  Propagation xs _ _ == Propagation ys _ _ = xs == ys
  {-# INLINE (==) #-}

instance Ord (Propagation p) where
  compare (Propagation as _ _) (Propagation bs _ _) =
    reverse as `compare` reverse bs
  {-# INLINE compare #-}

-- | Data type to encode mismatches during parsing or generation, kept
-- abstract. Use 'expected' and 'unexpected' constructors to build a
-- mismatch report.
data Mismatch = Mismatch
  { mismatchExpected :: Set Text
  , mismatchGot :: Maybe Text
  } deriving (Show, Eq)

-- | Construct a mismatch report with specified expectation. Can be
-- appended to other expectations and 'unexpected' reports to clarify
-- a mismatch.
expected :: Text -> Mismatch
expected a = Mismatch (S.singleton a) Nothing

-- | Construct a mismatch report with information what occurred during
-- the processing but was not expected.
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

runGrammar :: p -> ContextError (Propagation p) (GrammarError p) a -> Either (ErrorMessage p) a
runGrammar initPos m =
  case runContextError m (initPropagation initPos) of
    Left (GrammarError p mismatch) ->
      Left $ ErrorMessage
        (pPos p)
        (reverse (pAnns p))
        (mismatchExpected mismatch)
        (mismatchGot mismatch)
    Right a ->
      Right a

runGrammarDoc :: (Pretty p) => p -> ContextError (Propagation p) (GrammarError p) a -> Either (Doc ann) a
runGrammarDoc initPos m =
  left (ppError pretty) $
    runGrammar initPos m

runGrammarString :: (Show p) => p -> ContextError (Propagation p) (GrammarError p) a -> Either String a
runGrammarString initPos m =
  left (renderString . layoutSmart (LayoutOptions (AvailablePerLine 79 0.75)) . ppError (pretty . show)) $
    runGrammar initPos m


data ErrorMessage p = ErrorMessage
  { emPosition :: p
  , emAnnotations :: [Text]
  , emExpected :: Set Text
  , emGot :: Maybe Text
  }

ppMismatch :: Set Text -> Maybe Text -> Doc ann
ppMismatch (S.toList -> []) Nothing =
  "Unknown mismatch occurred"
ppMismatch (S.toList -> []) unexpected =
  "Unexpected:" <+> pretty unexpected
ppMismatch (S.toList -> expected) Nothing =
  "Expected:" <+> fillSep (punctuate comma $ map pretty expected)
ppMismatch (S.toList -> expected) (Just got) =
  vsep
  [ "Expected:" <+> fillSep (punctuate comma $ map pretty expected)
  , "But got: " <+> pretty got
  ]

ppError :: (p -> Doc ann) -> ErrorMessage p -> Doc ann
ppError ppPosition (ErrorMessage pos annotations expected got) =
  vsep $ catMaybes
    [ Just $ ppPosition pos `mappend` ":" <+> "mismatch:"
    , if null annotations
      then Nothing
      else Just $ indent 2 $ "In" <+> hsep (punctuate (comma <> line <> "in") $ map pretty annotations) <> colon
    , Just $ indent 4 $ ppMismatch expected got
    ]

data GrammarError p = GrammarError (Propagation p) Mismatch
  deriving (Show)

instance Semigroup (GrammarError p) where
  GrammarError pos m <> GrammarError pos' m'
    | pos > pos' = GrammarError pos m
    | pos < pos' = GrammarError pos' m'
    | otherwise  = GrammarError pos (m <> m')
  {-# INLINE (<>) #-}

doAnnotate :: MonadContextError (Propagation p) e m => Text -> m a -> m a
doAnnotate ann =
  localContext $ \propagation ->
    propagation { pAnns = ann : pAnns propagation }
{-# INLINE doAnnotate #-}

doDive :: MonadContextError (Propagation p) e m => m a -> m a
doDive =
  localContext $ \propagation ->
    propagation { pProp = 0 : pProp propagation }
{-# INLINE doDive #-}

doStep :: MonadContextError (Propagation p) e m => m ()
doStep =
  modifyContext $ \propagation ->
    propagation
      { pProp = case pProp propagation of
          (x : xs) -> succ x : xs
          [] -> [0]
      }
{-# INLINE doStep #-}

doLocate :: MonadContextError (Propagation p) e m => p -> m ()
doLocate pos =
  modifyContext $ \propagation ->
    propagation { pPos = pos }
{-# INLINE doLocate #-}

doError :: MonadContextError (Propagation p) (GrammarError p) m => Mismatch -> m a
doError mismatch =
  throwInContext $ \ctx ->
    GrammarError ctx mismatch
{-# INLINE doError #-}
