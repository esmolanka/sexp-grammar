{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.InvertibleGrammar.TH where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.InvertibleGrammar
import Data.InvertibleGrammar.Monad
import Data.Maybe
import Data.Set (singleton)
import Language.Haskell.TH as TH


{- | Build a prism and the corresponding grammar that will match on the
     given constructor and convert it to reverse sequence of :- stacks.

     E.g. consider a data type:

     > data FooBar a b c = Foo a b c | Bar

     For constructor Foo

     > fooGrammar = $(grammarFor 'Foo)

     will expand into

     > fooGrammar = PartialIso "Foo"
     >   (\(c :- b :- a :- t) -> Foo a b c :- t)
     >   (\case { Foo a b c :- t -> Just $ c :- b :- a :- t; _ -> Nothing })

     Note the order of elements on the stack:

     > ghci> :t fooGrammar
     > fooGrammar :: Grammar g (c :- (b :- (a :- t))) (FooBar a b c :- t)
-}
grammarFor :: Name -> ExpQ
grammarFor constructorName = do
  DataConI realConstructorName _typ parentName _fixity <- reify constructorName
  TyConI dataDef <- reify parentName

  let Just (single, constructorInfo) = do
        (single, allConstr) <- constructors dataDef
        constr <- findConstructor realConstructorName allConstr
        return (single, constr)

  let ts = fieldTypes constructorInfo
  vs <- mapM (const $ newName "x") ts
  t <- newName "t"

  let matchStack []      = varP t
      matchStack (_v:vs) = [p| $(varP _v) :- $_vs' |]
        where
          _vs' = matchStack vs
      fPat  = matchStack vs
      buildConstructor = foldr (\v acc -> appE acc (varE v)) (conE realConstructorName) vs
      fBody = [e| $buildConstructor :- $(varE t) |]
      fFunc = lamE [fPat] fBody

  let gPat  = [p| $_matchConsructor :- $(varP t) |]
        where
          _matchConsructor = conP realConstructorName (map varP (reverse vs))
      gBody = foldr (\v acc -> [e| $(varE v) :- $acc |]) (varE t) vs
      gFunc = lamCaseE $ catMaybes
        [ Just $ TH.match gPat (normalB [e| Right ($gBody) |]) []
        , if single
          then Nothing
          else Just $ TH.match wildP (normalB [e| Left $ Mismatch (singleton $(stringE (show constructorName))) Nothing |]) []
        ]

  [e| PartialIso $(stringE (show constructorName)) $fFunc $gFunc |]


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

----------------------------------------------------------------------
-- Utils

constructors :: Dec -> Maybe (Bool, [Con])
constructors (DataD _ _ _ cs _)   = Just (length cs == 1, cs)
constructors (NewtypeD _ _ _ c _) = Just (True, [c])
constructors _                    = Nothing

findConstructor :: Name -> [Con] -> Maybe Con
findConstructor _ [] = Nothing
findConstructor name (c:cs)
  | constructorName c == name = Just c
  | otherwise = findConstructor name cs

constructorName :: Con -> Name
constructorName con =
  case con of
    NormalC name _ -> name
    RecC name _ -> name
    InfixC _ name _ -> name
    ForallC _ _ con' -> constructorName con'

fieldTypes :: Con -> [Type]
fieldTypes (NormalC _ fieldTypes) = map snd fieldTypes
fieldTypes (RecC _ fieldTypes) = map (\(_, _, t) ->t) fieldTypes
fieldTypes (InfixC (_,a) _b (_,b)) = [a, b]
fieldTypes (ForallC _ _ con') = fieldTypes con'
