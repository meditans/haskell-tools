{-# LANGUAGE KindSignatures
           , TypeOperators
           , DataKinds
           , PolyKinds
           , TypeFamilies
           , UndecidableInstances
           #-}
module Language.Haskell.Tools.AST.ClassyPlate.TypePrune (ClassIgnoresSubtree, AppSelector, IgnoredFields) where

import GHC.Exts (Constraint)
import GHC.Generics
import Data.Type.Bool
import Data.Type.List
import GHC.TypeLits (Symbol)

-- | This type decides if the subtree of an element cannot contain an element that is transformed.
type family ClassIgnoresSubtree (cls :: * -> Constraint) (typ :: *) :: Bool where
  ClassIgnoresSubtree cls typ = Not (AnySelected cls (MemberTypes typ))

-- | Instantiate this type family to signal what elements does your operation operate on.
-- If @AppSelector c t@ is True, there should be a @c t@ instance. AppSelector should be
-- a total type function for a given class, at least for all the types that can possibly
-- accessed.
type family AppSelector (c :: * -> Constraint) (a :: *) :: Bool

-- | This type family sets which fields should not be traversed when trying to generate
-- automatically pruned versions of classy traversal.
type family IgnoredFields (t :: *) :: [Symbol]

type family AnySelected (c :: * -> Constraint) (ls :: [*]) :: Bool where
  AnySelected c (fst ': rest) = AppSelector c fst || AnySelected c rest
  AnySelected c '[] = False

type family MemberTypes (typ :: *) :: [*] where
  MemberTypes t = GetMemberTypes '[] t

type family GetMemberTypes (checked :: [*]) (typ :: *) :: [*] where 
  -- primitive types without Rep instance
  GetMemberTypes checked Char = '[Char]
  GetMemberTypes checked Int = '[Int]
  GetMemberTypes checked Float = '[Float]
  GetMemberTypes checked Double = '[Double]
  GetMemberTypes checked t = GetElementTypes t checked (Rep t)

type family GetElementTypes (t :: *) (checked :: [*]) (typ :: * -> *) :: [*] where 
  GetElementTypes t checked (D1 md cons) = GetElementTypesCons t checked cons

type family GetElementTypesCons (t :: *) (checked :: [*]) (typ :: * -> *) where 
  GetElementTypesCons t checked (C1 mc flds) = GetElementTypesFields t checked flds
  GetElementTypesCons t checked (c1 :+: c2) = GetElementTypesCons t checked c1 `Union` GetElementTypesCons t checked c2

type family GetElementTypesFields (t :: *) (checked :: [*]) (typ :: * -> *) where 
  GetElementTypesFields t checked (fld1 :*: fld2) = GetElementTypesFields t checked fld1 `Union` GetElementTypesFields t checked fld2
  GetElementTypesFields t checked (S1 (MetaSel (Just fld) unp str laz) (Rec0 innerT)) 
    = If (Find fld (IgnoredFields t)) '[] (GetElementTypesField checked (Find innerT checked) innerT)
  GetElementTypesFields t checked (S1 ms (Rec0 innerT)) = GetElementTypesField checked (Find innerT checked) innerT
  GetElementTypesFields t checked U1 = '[]

type family GetElementTypesField (checked :: [*]) (inChecked :: Bool) (typ :: *) where 
  GetElementTypesField checked True typ = '[]
  GetElementTypesField checked False typ = Insert typ (GetMemberTypes (typ ': checked) typ)
