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
grammarFor conName = [e| GenPrism $(stringE (show conName)) $(deriveRevStackPrism conName) |]

{- | Build prisms and corresponding grammars for all data constructors of given
     type. Expects grammars to zip built ones with.

     > $(match ''Maybe)

     Will expand into a lambda:

     > (\nothingG justG -> ($(grammarFor 'Nothing) . nothingG) <>
     >                     ($(grammarFor 'Just)    . justG))
-}
match :: Name -> ExpQ
match tyName = do
  names <- map constructorName . extractConstructors <$> reify tyName
  argTys <- mapM (\_ -> newName "a") names
  let grammars = map (\(con, arg) -> [e| $(varE arg) $(grammarFor con) |]) (zip names argTys)
  lamE (map varP argTys) (foldr1 (\e1 e2 -> [e| $e1 :<>: $e2 |]) grammars)
  where
    extractConstructors :: Info -> [Con]
    extractConstructors info =
      case info of
        TyConI (DataD _ _ _ cons _) -> cons
        TyConI (NewtypeD _ _ _ con _) -> [con]
        _ -> error "Type name is expected"
    constructorName :: Con -> Name
    constructorName con =
      case con of
        NormalC name _ -> name
        RecC name _ -> name
        InfixC _ name _ -> name
        ForallC _ _ con' -> constructorName con'
