{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.SexpGrammar.Parser where

import Control.Applicative
#if MIN_VERSION_mtl(2, 2, 0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

data Result a
  = Success a
  | Failure String
  deriving (Functor)

instance Applicative Result where
  pure = Success
  Success f <*> Success a = Success (f a)
  Failure a <*> Success _ = Failure a
  Success _ <*> Failure b = Failure b
  Failure a <*> Failure b = Failure $ a ++ "\n" ++ b

instance Monad Result where
  return = Success
  Failure a >>= _ = Failure a
  Success a >>= f = f a

instance Alternative Result where
  empty = Failure "empty"
  Success a <|> _ = Success a
  Failure _ <|> Success b = Success b
  Failure a <|> Failure b = Failure (a ++ "\nor\n" ++ b)

instance MonadPlus Result

instance MonadError [Char] Result where
  throwError = Failure
  catchError res handle =
    case res of
      Success a -> Success a
      Failure b -> handle b

runParser :: (a -> Result b) -> a -> Either String b
runParser parser a =
  case parser a of
    Success a -> Right a
    Failure b -> Left b
