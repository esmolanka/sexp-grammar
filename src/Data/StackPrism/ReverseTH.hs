-- | Derive StackPrisms with TH that have reverse order of fields.

{-# LANGUAGE TemplateHaskell #-}

module Data.StackPrism.ReverseTH where

import Control.Applicative
import Data.List
import Language.Haskell.TH

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
  DataConI realName typ parentName _fixity <- reify constructorName
  TyConI dataDef                           <- reify parentName
  let (datatypeType, tvars, constructors) =
        case dataDef of
          DataD _ctx typeName tvars cs _csNames ->
            (mkDatatypeType typeName tvars, tvars, cs)
          NewtypeD _ctx typeName tvars c _ ->
            (mkDatatypeType typeName tvars, tvars, [c])
      conCount = length constructors
  return undefined
  where
    mkDatatypeType :: Name -> [TyVarBndr] -> TypeQ
    mkDatatypeType typeName tvars =
      foldl' appT (conT typeName) $ map (varT . tvarName) tvars
      where
        tvarName :: TyVarBndr -> Name
        tvarName (PlainTV name)    = name
        tvarName (KindedTV name _) = name
