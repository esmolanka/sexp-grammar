-- | Derive StackPrisms with TH that have reverse order of fields.

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wwarn #-}

module Data.StackPrism.ReverseTH where

import Control.Applicative
import Data.List
import Data.StackPrism
import Language.Haskell.TH

-- NB debug in ghci with
-- > putStrLn $(stringE . show =<< revStackPrism 'Foo )
-- and
-- > putStrLn $(stringE . pprint =<< revStackPrism 'Foo )

-- | Build Prism that will match on the given constructor and convert it
-- to reverse sequence of :- stacks.
--
-- E.g. for a datatype constructor
--
-- > data Foo a b c = Foo a b c
--
-- $(revStackPrism 'Foo)
--
-- will expand into
--
-- > stackPrism (\(c :- b :- a :- t) -> Foo a b c :- t) (\(Foo a b c :- t) -> Just $ c :- b :- a :- t)
--
revStackPrism :: Name -> Q Exp
revStackPrism constructorName = do
  DataConI realConstructorName typ parentName _fixity <- reify constructorName
  TyConI dataDef                           <- reify parentName

  Just constructorInfo <- return $ findConstructor realConstructorName =<< constructors dataDef
  let ts = fieldTypes constructorInfo
  vs <- mapM (const $ newName "x") ts

  t <- newName "t"

  Just pairConstructor <- lookupValueName ":-"
  Just stackPrismFunc <- lookupValueName "stackPrism"
  -- Build "f" and "g" for "stackPrism f g"
  let gPat  = conP pairConstructor [conP realConstructorName (map varP vs), varP t]
      gBody = foldr (\v acc -> appsE [conE pairConstructor, varE v, acc]) (varE t) $ reverse vs

  lamCaseE [match gPat (normalB [e| Just $ $gBody |]) [], match wildP (normalB [e| Nothing |]) []]
  where
    mkDatatypeType :: Name -> [TyVarBndr] -> TypeQ
    mkDatatypeType typeName tvars =
      foldl' appT (conT typeName) $ map (varT . tvarName) tvars
      where
        tvarName :: TyVarBndr -> Name
        tvarName (PlainTV name)    = name
        tvarName (KindedTV name _) = name

constructors :: Dec -> Maybe [Con]
constructors (DataD _ _ _ cs _)   = Just cs
constructors (NewtypeD _ _ _ c _) = Just [c]

findConstructor :: Name -> [Con] -> Maybe Con
findConstructor _    [] = Nothing
findConstructor name (c:cs)
  | constructorName c == name = Just c
  | otherwise                 = findConstructor name cs

constructorName :: Con -> Name
constructorName (NormalC name _)  = name
constructorName (RecC name _)     = name
constructorName (InfixC _ name _) = name
constructorName (ForallC _ _ c)   = constructorName c

fieldTypes :: Con -> [Type]
fieldTypes (NormalC _ fieldTypes) = map snd fieldTypes
fieldTypes (RecC _ fieldTypes) = map (\(_, _, t) ->t) fieldTypes
