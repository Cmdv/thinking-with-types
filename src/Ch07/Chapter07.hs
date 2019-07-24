{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}


module Ch07.Chapter07 where

import           Data.Foldable
import           Data.IORef
import           Data.Kind        (Constraint, Type)
import           Data.Maybe
import           Data.Typeable
import           System.IO.Unsafe (unsafePerformIO)

-- Existential Types

-- data Any = forall a. Any a

data Any where
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

-- Exercise 7.1-i
-- What is interesting about (forall a. a -> r) ?
-- They can only return constant values as the polymorphism in
-- their input doesn't allow inspection.

data HasShow where
  HasShow :: Show t => t -> HasShow

-- instance Show HasShow where
--   show (HasShow s) = "hasShow " ++ show s

-- Exercise 7.1-ii
-- What happens to this instance if you remove the Show t => constraint from HasShow?
-- A type error on `show` of:
-- No instance for (Show t) arising from a use of ‘show’

elimHasShow
  :: (forall a. Show a => a -> r)
  -> HasShow
  -> r
elimHasShow f (HasShow a) = f a

instance Show HasShow where
  show = elimHasShow show

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic
  :: (forall a. Typeable a => a -> r)
  -> Dynamic
  -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
  :: forall a b r.
     ( Typeable a
     , Typeable b
     , Typeable r
     )
   => Dynamic
   -> Dynamic
   -> (a -> b -> r)
   -> Maybe Dynamic
liftD2 d1 d2 f =
  fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2


pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
  [ liftD2 @String @String a b (++)
  , liftD2 @Int @Int a b (+)
  , liftD2 @String @Int a b (\strA intB -> strA ++ show intB)
  , liftD2 @Int @String a b (\intA stringB -> show intA ++ stringB)
  ]

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r
elimHas f (Has a) = f a

type HasShow' = Has Show
type Dynamic' = Has Typeable

isEmpty :: (Monoid a, Eq a) => a -> Bool
isEmpty a = a == mempty

class    (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a


-- ST Type

newtype ST s a = ST
  {
    unsafeRunST :: a
  }

newtype STRef s a = STRef
  {
    unSTRef :: IORef a
  }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  return = pure
  ST a >>= f = seq a $ f a

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref =
  pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a
