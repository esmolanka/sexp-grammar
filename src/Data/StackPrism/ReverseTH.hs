-- | Derive StackPrisms with TH that have reverse order of fields.

{-# LANGUAGE TemplateHaskell #-}

module Data.StackPrism.ReverseTH
  ( deriveRevStackPrism
  ) where

import Data.StackPrism
import Language.Haskell.TH

{- | Build Prism that will match on the given constructor and convert it
     to reverse sequence of :- stacks.

     E.g. for a datatype constructor

     > data Foo a b c = Foo a b c

     $(revStackPrism 'Foo)

     will expand into

     > stackPrism (\(c :- b :- a :- t) -> Foo a b c :- t)
     >            (\(Foo a b c :- t)   -> Just $ c :- b :- a :- t)
-}

deriveRevStackPrism :: Name -> Q Exp
deriveRevStackPrism constructorName = do
  DataConI realConstructorName _typ parentName _fixity <- reify constructorName
  TyConI dataDef <- reify parentName

  Just constructorInfo <- return $ findConstructor realConstructorName =<< constructors dataDef
  let ts = fieldTypes constructorInfo
  vs <- mapM (const $ newName "x") ts
  t <- newName "t"

  let matchStack []     = varP t
      matchStack (v:vs) = [p| ($(varP v) :- $(matchStack vs)) |]
      fPat  = matchStack vs
      buildConstructor = foldr (\v acc -> appE acc (varE v)) (conE realConstructorName) vs
      fBody = [e| $buildConstructor :- $(varE t) |]
      fFunc = lamE [fPat] fBody

  let matchConsructor = conP realConstructorName (map varP (reverse vs))
      gPat  = [p| $matchConsructor :- $(varP t) |]
      gBody = foldr (\v acc -> [e| $(varE v) :- $acc |]) (varE t) vs
      gFunc = lamCaseE [ match gPat (normalB [e| Just $ $gBody |]) []
                       , match wildP (normalB [e| Nothing |]) []
                       ]

  [e| stackPrism $fFunc $gFunc|]

constructors :: Dec -> Maybe [Con]
constructors (DataD _ _ _ cs _)   = Just cs
constructors (NewtypeD _ _ _ c _) = Just [c]
constructors _ = error "Data type declaration expected"

findConstructor :: Name -> [Con] -> Maybe Con
findConstructor _ [] = Nothing
findConstructor name (c:cs)
  | constructorName c == name = Just c
  | otherwise = findConstructor name cs

constructorName :: Con -> Name
constructorName (NormalC name _)  = name
constructorName (RecC name _)     = name
constructorName (InfixC _ name _) = name
constructorName (ForallC _ _ _)   = error "ForallC constructors not supported"

fieldTypes :: Con -> [Type]
fieldTypes (NormalC _ fieldTypes) = map snd fieldTypes
fieldTypes (RecC _ fieldTypes) = map (\(_, _, t) ->t) fieldTypes
fieldTypes (InfixC (_,a) _b (_,b)) = [a, b]
fieldTypes (ForallC _ _ _) = error "ForallC constructors not supported"
