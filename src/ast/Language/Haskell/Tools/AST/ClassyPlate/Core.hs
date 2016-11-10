{-# LANGUAGE Rank2Types
           , ConstraintKinds
           , KindSignatures
           , TypeFamilies
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , AllowAmbiguousTypes
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , DataKinds
           , TypeApplications
           , DeriveGeneric
           , DeriveDataTypeable
           , TypeOperators
           , PolyKinds 
           #-}
module Language.Haskell.Tools.AST.ClassyPlate.Core
  ( -- public functions and classes
    classyTraverse, classyTraverseM, selectiveTraverse, selectiveTraverseM, smartTraverse, smartTraverseM
  , ClassyPlate, SmartClassyPlate
    -- generator functions and datatypes
  , classyTraverse_, classyTraverseM_, selectiveTraverse_, selectiveTraverseM_, smartTraverse_, smartTraverseM_
  , app, appM, appIf, appIfM
  , GoodOperationFor, GoodOperationForAuto, FlagToken
    -- MonoMatch
  , MonoMatch (..)

  ) where

import GHC.Exts
import Data.Maybe
import GHC.Generics (Generic)
import Data.Data (Data)

import Language.Haskell.Tools.AST.ClassyPlate.TypePrune

-- FIXME: when TH supports type application we can remove the token parameters

type GoodOperationFor c e = (App (AppSelector c e) c e)
type GoodOperationForAuto c e = (GoodOperationFor c e, Generic e)

data ClsToken (c :: * -> Constraint)
data FlagToken (c :: Bool)

-- | A class for applying a function if the class of the functions allows the application
class App (flag :: Bool) c b where
  app :: FlagToken flag -> ClsToken c -> (forall a . c a => a -> a) -> b -> b
  appM :: Monad m => FlagToken flag -> ClsToken c -> (forall a . c a => a -> m a) -> b -> m b
  appPred :: FlagToken flag -> ClsToken c -> (forall a . c a => a -> Bool) -> b -> b -> b
  appPredM :: Monad m => FlagToken flag -> ClsToken c -> (forall a . c a => a -> m Bool) -> b -> m b -> m b

instance c b => App 'True c b where
  {-# INLINE app #-}
  app _ _ f a = f a
  {-# INLINE appM #-}
  appM _ _ f a = f a
  {-# INLINE appPred #-}
  appPred _ _ f th el = if f th then el else th
  {-# INLINE appPredM #-}
  appPredM _ _ f th el = f th >>= \p -> if p then el else return th

instance App 'False c b where
  {-# INLINE app #-}
  app _ _ _ a = a
  {-# INLINE appM #-}
  appM _ _ _ a = return a
  {-# INLINE appPred #-}
  appPred _ _ _ _ el = el
  {-# INLINE appPredM #-}
  appPredM _ _ _ _ el = el

-- | A class for traversals that use a polymorphic function to visit all applicable elements.
class GoodOperationFor c b => ClassyPlate c b where
  classyTraverse_ :: ClsToken c -> (forall a . c a => a -> a) -> b -> b
  classyTraverseM_ :: Monad m => ClsToken c -> (forall a . c a => a -> m a) -> b -> m b

  selectiveTraverse_ :: ClsToken c -> (forall a . c a => a -> a) -> (forall a . c a => a -> Bool) -> b -> b
  selectiveTraverseM_ :: Monad m => ClsToken c -> (forall a . c a => a -> m a) -> (forall a . c a => a -> m Bool) -> b -> m b

-- | A class for traversals that use a polymorphic function to visit all applicable elements but only visit the 
-- parts where the applicable elements could be found.
class (GoodOperationForAuto c b) => SmartClassyPlate c (sel :: Bool) b where
  smartTraverse_ :: FlagToken sel -> ClsToken c -> (forall a . c a => a -> a) -> b -> b
  smartTraverseM_ :: Monad m => FlagToken sel -> ClsToken c -> (forall a . c a => a -> m a) -> b -> m b

-- | Traverse the data structure with a polymorphic function.
classyTraverse :: forall c b . ClassyPlate c b => (forall a . c a => a -> a) -> b -> b
{-# INLINE classyTraverse #-}
classyTraverse = classyTraverse_ (undefined :: ClsToken c)

-- | Traverse the data structure with a polymorphic monadic function.
classyTraverseM :: forall c b m . (ClassyPlate c b, Monad m) => (forall a . c a => a -> m a) -> b -> m b
{-# INLINE classyTraverseM #-}
classyTraverseM = classyTraverseM_ (undefined :: ClsToken c)

-- | Traverse only those parts that are selected by the given selector function. 
selectiveTraverse :: forall c b . ClassyPlate c b => (forall a . c a => a -> a) -> (forall a . c a => a -> Bool) -> b -> b
{-# INLINE selectiveTraverse #-}
selectiveTraverse = selectiveTraverse_ (undefined :: ClsToken c)

-- | Traverse only those parts that are selected by the given monadic selector function.
selectiveTraverseM :: forall c b m . (ClassyPlate c b, Monad m) => (forall a . c a => a -> m a) -> (forall a . c a => a -> m Bool) -> b -> m b
{-# INLINE selectiveTraverseM #-}
selectiveTraverseM = selectiveTraverseM_ (undefined :: ClsToken c)

-- | Traverse only those parts of the data structure that could possibly contain elements that the given function can be applied on
smartTraverse :: forall c b . SmartClassyPlate c (ClassIgnoresSubtree c b) b => (forall a . c a => a -> a) -> b -> b
{-# INLINE smartTraverse #-}
smartTraverse = smartTraverse_ (undefined :: FlagToken (ClassIgnoresSubtree c b)) (undefined :: ClsToken c)

-- | Traverse only those parts of the data structure that could possibly contain elements that the given monadic function can be applied on
smartTraverseM :: forall c b m . (SmartClassyPlate c (ClassIgnoresSubtree c b) b, Monad m) => (forall a . c a => a -> m a) -> b -> m b
{-# INLINE smartTraverseM #-}
smartTraverseM = smartTraverseM_ (undefined :: FlagToken (ClassIgnoresSubtree c b)) (undefined :: ClsToken c)

instance (GoodOperationForAuto c b) => SmartClassyPlate c True b where
  smartTraverse_ _ t f a = app (undefined :: FlagToken (AppSelector c b)) t f a
  {-# INLINE smartTraverse_ #-}
  smartTraverseM_ _ t f a = appM (undefined :: FlagToken (AppSelector c b)) t f a
  {-# INLINE smartTraverseM_ #-}

{-# SPECIALIZE INLINE classyTraverse_ :: ClassyPlate (MonoMatch x) sel b => (forall a . MonoMatch x a => a -> a) -> b -> b #-}

-- | A class for the simple case when the applied function is monomorphic.
class MonoMatch a b where
  -- | Apply a monomorphic function on a polymorphic data structure.
  monoApp :: (a -> a) -> b -> b

instance MonoMatch a a where
  monoApp = id
  {-# INLINE monoApp #-}

type instance AppSelector (MonoMatch a) b = TypEq a b

type family TypEq a b :: Bool where
  TypEq a a = 'True
  TypEq a b = 'False

appIf :: forall c b . App (AppSelector c b) c b => ClsToken c -> (forall a . c a => a -> a) -> (forall a . c a => a -> Bool) -> b -> b -> b
{-# INLINE appIf #-}
appIf t f pred val combined = app flTok t f $ appPred flTok t pred val combined
  where flTok = undefined :: FlagToken (AppSelector c b)

appIfM :: forall c b m . (App (AppSelector c b) c b, Monad m) => ClsToken c -> (forall a . c a => a -> m a) -> (forall a . c a => a -> m Bool) -> b -> m b -> m b
{-# INLINE appIfM #-}
appIfM t f pred val combined = appM flTok t f =<< appPredM flTok t pred val combined
  where flTok = undefined :: FlagToken (AppSelector c b)