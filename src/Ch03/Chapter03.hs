module Ch03.Chapter03 where

import           Data.Functor.Contravariant (Contravariant, contramap)

-- Exersise 3-i

newtype T1 a = T1 (Int -> a)

newtype T2 a = T2 (a -> Int)

instance Functor T1 where
  fmap f (T1 a) = T1 $ f . a

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 aii) = T5 $ \ii -> aii $ ii . f


-- Examples of contramap
newtype Predicate' a = Predicate' (a -> Bool)

instance Contravariant Predicate' where
  contramap f (Predicate' fa) = Predicate' $ fa . f

isPredicateFive :: Predicate' Int
isPredicateFive = Predicate' (== 5)

data MyType = MyType
  { intValue :: Int
  , blah     :: String
  }

myPredicate :: Predicate' MyType
myPredicate = contramap intValue isPredicateFive

test :: Predicate' a -> a -> Bool
test (Predicate' fn) a = fn a

-- > test isPredicateFive 5
-- True
-- > test isPredicateFive 6
-- False
-- > test myPredicate $ MyType 5 "hello"
-- True
-- > test myPredicate $ MyType 6 "nope"
-- False
