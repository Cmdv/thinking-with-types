{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Ch05.Chapter05 where

import           Data.Kind (Constraint, Type)
-- type Equality :
-- reflexivity - a type is always equal to itself: `a ~ a`
-- symmetry - `a ~ b` holds if and inly if `b ~ a`

data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  Not     :: Expr Bool -> Expr Bool
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt i)  = i
evalExpr (LitBool a) = a
evalExpr (Add x y)    = evalExpr x + evalExpr y
evalExpr (Not x)      = not $ evalExpr x
evalExpr (If b x y)   =
  if evalExpr b
    then evalExpr x
    else evalExpr y


data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# xs) = 1 + hLength xs

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# HNil) = show b

-- for empty HList
-- instance Eq (HList '[]) where
--   HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--   (a :# as) == (b :# bs) = a == b && as == bs

-- Exersise - 5.3-i

-- instance Ord (HList '[]) where
--   compare HNil HNil = EQ

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--   compare (a :# as) (b :# bs) = compare a b <> compare as bs


-- Exersise - 5.3-ii

-- instance Show (HList '[]) where
--   show HNil = "HNil"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--   show (a :# as) = show a <> " :# " <> show as

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[]       = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
  All c '[]       = ()
  All c (t ': ts) = (c t, All c ts)


instance All Eq ts => Eq (HList ts) where
  HNil      == HNil      = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- Exersise - 5.3-iii

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
   compare HNil HNil = EQ
   compare (a :# as) (b :# bs) = compare a b <> compare as bs


instance (All Show ts) => Show (HList ts) where
  show HNil      = "HNil"
  show (a :# as) = show a <> " :# " <> show as
