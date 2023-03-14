{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.InvertibleGrammar.TH where

import Data.Foldable (toList)
import Data.InvertibleGrammar.Base
import Data.Maybe
import Data.Text (pack)
import Language.Haskell.TH as TH
import Data.Set (Set)
import qualified Data.Set as S


{- | Build a prism and the corresponding grammar that will match on the
     given constructor and convert it to reverse sequence of :- stacks.

     E.g. consider a data type:

     > data FooBar a b c = Foo a b c | Bar

     For constructor Foo

     > fooGrammar = $(grammarFor 'Foo)

     will expand into

     > fooGrammar = PartialIso
     >   (\(c :- b :- a :- t) -> Foo a b c :- t)
     >   (\case { Foo a b c :- t -> Just $ c :- b :- a :- t; _ -> Nothing })

     Note the order of elements on the stack:

     > ghci> :t fooGrammar
     > fooGrammar :: Grammar p (c :- (b :- (a :- t))) (FooBar a b c :- t)
-}

grammarFor :: Name -> ExpQ
grammarFor constructorName = do
  DataConI realConstructorName _typ parentName <- reify constructorName
  TyConI dataDef <- reify parentName

  (single, constructorInfo) <- maybe (fail "Could not find the constructor") pure $ do
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
          else Just $ TH.match wildP (normalB [e| Left (expected $ "constructor " <> pack ( $(stringE (show constructorName))) ) |]) []
        ]

  [e| PartialIso $fFunc $gFunc |]


{- | Build prisms and corresponding grammars for all data constructors of given
     type. Expects grammars to zip built ones with.

     > $(match ''Maybe)

     Will expand into a lambda:

     > (\nothingG justG -> ($(grammarFor 'Nothing) . nothingG) <>
     >                     ($(grammarFor 'Just)    . justG))
-}
match :: Name -> ExpQ
match tyName = do
  names  <- concatMap (toList . constructorNames) <$> (extractConstructors =<< reify tyName)
  argTys <- mapM (\_ -> newName "a") names
  let grammars = map (\(con, arg) -> [e| $(varE arg) $(grammarFor con) |]) (zip names argTys)
  lamE (map varP argTys) (foldr1 (\e1 e2 -> [e| $e1 <> $e2 |]) grammars)
  where
    extractConstructors :: Info -> Q [Con]
    extractConstructors (TyConI dataDef) =
      case constructors dataDef of
        Just (_, cs) -> pure cs
        Nothing      -> fail $ "Data type " ++ show tyName ++ " defines no constructors"
    extractConstructors _ =
      fail $ "Data definition expected for name " ++ show tyName

----------------------------------------------------------------------
-- Utils

constructors :: Dec -> Maybe (Bool, [Con])
constructors (DataD _ _ _ _ cs _)   = Just (length cs == 1, cs)
constructors (NewtypeD _ _ _ _ c _) = Just (True, [c])
constructors _                      = Nothing

findConstructor :: Name -> [Con] -> Maybe Con
findConstructor _    [] = Nothing
findConstructor name (c:cs)
  | name `S.member` constructorNames c = Just c
  | otherwise                          = findConstructor name cs

constructorNames :: Con -> Set Name
constructorNames = \case
  NormalC name _   -> S.singleton name
  RecC name _      -> S.singleton name
  InfixC _ name _  -> S.singleton name
  ForallC _ _ con' -> constructorNames con'
  GadtC cs _ _     -> S.fromList cs
  RecGadtC cs _ _  -> S.fromList cs

fieldTypes :: Con -> [Type]
fieldTypes = \case
  NormalC _ fieldTypes  -> map extractType fieldTypes
  RecC _ fieldTypes     -> map extractType' fieldTypes
  InfixC (_,a) _b (_,b) -> [a, b]
  ForallC _ _ con'      -> fieldTypes con'
  GadtC _ fs _          -> map extractType fs
  RecGadtC _ fs _       -> map extractType' fs
  where
    extractType (_, t) = t
    extractType' (_, _, t) = t
