{-# LANGUAGE TemplateHaskell #-}
module Data.InvertibleGrammar.TH where

import Language.Haskell.TH
import Data.StackPrism.ReverseTH
import Data.InvertibleGrammar

{- | Build a prism and the corresponding grammar that will match on the
     given constructor and convert it to reverse sequence of :- stacks.

     E.g. consider a data type:

     > data FooBar a b c = Foo a b c | Bar

     For constructor Foo

     > fooGrammar = $(grammarFor 'Foo)

     will expand into

     > fooGrammar = GenPrism "Foo" $
     >  stackPrism
     >   (\(c :- b :- a :- t) -> Foo a b c :- t)
     >   (\case { Foo a b c :- t -> Just $ c :- b :- a :- t; _ -> Nothing })

     Note the order of elements on the stack:

     > ghci> :t fooGrammar
     > fooGrammar :: Grammar g (c :- (b :- (a :- t))) (FooBar a b c :- t)
-}
grammarFor :: Name -> ExpQ
grammarFor name = [e| GenPrism $(stringE (show name)) $(deriveRevStackPrism name) |]
