{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeOperators         #-}

module Data.InvertibleGrammar.Base
  ( Grammar (..)
  , (:-) (..)
  , forward
  , backward
  , GrammarError (..)
  , Mismatch
  , expected
  , unexpected
  ) where

import Prelude hiding ((.), id)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Category
import Control.Monad
import Data.Text (Text)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable
import Data.Foldable
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.InvertibleGrammar.Monad
import qualified Debug.Trace

-- | \"Cons\" pair of a heterogenous list or a stack with potentially
-- polymophic tail. E.g. @"first" :- 2 :- (3,4) :- t@
--
-- Isomorphic to a tuple with two elments, but is much more
-- convenient for nested pairs.
data h :- t = h :- t deriving (Eq, Show, Functor, Foldable, Traversable)
infixr 5 :-

instance Bifunctor (:-) where
  bimap f g (a :- b) = f a :- g b

instance Bifoldable (:-) where
  bifoldr f g x0 (a :- b) = a `f` (b `g` x0)

instance Bitraversable (:-) where
  bitraverse f g (a :- b) = (:-) <$> f a <*> g b

-- | Representation of an invertible grammar -- a grammar that can be
-- run either "forwards" and "backwards".
--
-- For a grammar @Grammar p a b@, running it forwards will take a
-- value of type @a@ and possibly produce a value of type @b@. Running
-- it backwards will take a value of type @b@ and possibly produce an
-- @a@. If a value cannot be produced, an error message is generated.
--
-- As a common example, running a 'Grammar' forwards corresponds to
-- parsing and running backwards corresponds to prettyprinting.
--
-- That is, the grammar defines a partial isomorphism between two
-- values.
data Grammar p a b where
  -- | Total isomorphism grammar.
  Iso        :: (a -> b) -> (b -> a) -> Grammar p a b

  -- | Partial isomorphism. Use 'Flip' to change the direction of
  -- partiality.
  PartialIso :: (a -> b) -> (b -> Either Mismatch a) -> Grammar p a b

  -- | Flip forward and backward passes of an underlying grammar.
  Flip       :: Grammar p a b -> Grammar p b a

  -- | Grammar composition.
  (:.:)      :: Grammar p b c -> Grammar p a b -> Grammar p a c

  -- | Grammar alternation. Left operand is tried first.
  (:<>:)     :: Grammar p a b -> Grammar p a b -> Grammar p a b

  -- | Application of a grammar on 'Traversable' functor.
  Traverse   :: (Traversable f) => Grammar p a b -> Grammar p (f a) (f b)

  -- | Applicaiton of a grammar on stack head
  -- (first component of ':-').
  OnHead     :: Grammar p a b -> Grammar p (a :- t) (b :- t)

  -- | Applicaiton of a grammar on stack tail
  -- (second component of ':-').
  OnTail     :: Grammar p a b -> Grammar p (h :- a) (h :- b)

  -- | Application of a grammar inside a context of annotation, used
  -- for error messages.
  Annotate   :: Text -> Grammar p a b -> Grammar p a b

  -- | Application of a grammar inside a context of a nested
  -- structure, used for error messages. E.g. JSON arrays.
  Dive       :: Grammar p a b -> Grammar p a b

  -- | Propagate logical position inside a nested
  -- structure. E.g. after each successfully matched element of a JSON
  -- array.
  Step       :: Grammar p a a

  -- | Update the position of grammar monad from value on grammar's
  -- input or output on forward or backward pass, respectively. Used
  -- for error messages.
  Locate     :: Grammar p p p

trace :: String -> a -> a
trace = if False then Debug.Trace.trace else flip const


instance Category (Grammar p) where
  id                                              = Iso id id

  PartialIso f g        . Iso f' g'               = trace "p/i" $ PartialIso (f . f') (fmap g' . g)
  Iso f g               . PartialIso f' g'        = trace "i/p" $ PartialIso (f . f') (g' . g)

  Flip (PartialIso f g) . Iso f' g'               = trace "fp/i" $ Flip $ PartialIso (g' . f) (g . f')
  Iso f g               . Flip (PartialIso f' g') = trace "i/fp" $ Flip $ PartialIso (f' . g) (fmap f . g')

  PartialIso f g        . (Iso f' g'               :.: h) = trace "p/i2" $ PartialIso (f . f') (fmap g' . g) :.: h
  Iso f g               . (PartialIso f' g'        :.: h) = trace "i/p2" $ PartialIso (f . f') (g' . g) :.: h

  Flip (PartialIso f g) . (Iso f' g'               :.: h) = trace "fp/i2" $ Flip (PartialIso (g' . f) (g . f')) :.: h
  Iso f g               . (Flip (PartialIso f' g') :.: h) = trace "i/fp2" $ Flip (PartialIso (f' . g) (fmap f . g')) :.: h

  Flip g . Flip h                                 = trace "f/f" $ Flip (h . g)
  Iso f g . Iso f' g'                             = trace "i/i" $ Iso (f . f') (g' . g)

  (g :.: h)             . j                       = trace "assoc" $ g :.: (h . j)

  g                     . h                       = g :.: h


instance Semigroup (Grammar p a b) where
  (<>) = (:<>:)

-- | Run 'Grammar' forwards.
--
-- For @Grammar p a b@, given a value of type @a@ tries to produce a
-- value of type @b@, otherwise reports an error with position of type
-- @p@.
forward :: Grammar p a b -> a -> ContextError (Propagation p) (GrammarError p) b
forward (Iso f _)        = return . f
forward (PartialIso f _) = return . f
forward (Flip g)         = backward g
forward (g :.: f)        = forward g <=< forward f
forward (f :<>: g)       = \x -> forward f x `mplus` forward g x
forward (Traverse g)     = traverse (forward g)
forward (OnHead g)       = \(a :- b) -> (:- b) <$> forward g a
forward (OnTail g)       = \(a :- b) -> (a :-) <$> forward g b
forward (Annotate t g)   = doAnnotate t . forward g
forward (Dive g)         = doDive . forward g
forward Step             = \x -> doStep >> return x
forward Locate           = \x -> doLocate x >> return x

-- | Run 'Grammar' backwards.
--
-- For @Grammar p a b@, given a value of type @b@ tries to produce a
-- value of type @a@, otherwise reports an error with position of type
-- @p@.
backward :: Grammar p a b -> b -> ContextError (Propagation p) (GrammarError p) a
backward (Iso _ g)        = return . g
backward (PartialIso _ g) = either doError return . g
backward (Flip g)         = forward g
backward (g :.: f)        = backward g >=> backward f
backward (f :<>: g)       = \x -> backward f x `mplus` backward g x
backward (Traverse g)     = traverse (backward g)
backward (OnHead g)       = \(a :- b) -> (:- b) <$> backward g a
backward (OnTail g)       = \(a :- b) -> (a :-) <$> backward g b
backward (Annotate t g)   = doAnnotate t . backward g
backward (Dive g)         = doDive . backward g
backward Step             = \x -> doStep >> return x
backward Locate           = \x -> doLocate x >> return x
