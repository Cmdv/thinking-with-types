{-# LANGUAGE RankNTypes #-}
module Ch06.Chapter06 where

-- Rank-N Types

-- applyToFive :: (a -> a) -> Int
-- applyToFive f = f 5

id' :: forall a. a -> a
id' a = a

-- putting the forall inside parens restricting the polymorphism only to the function
applyTofive :: (forall a. a -> a) -> Int
applyTofive f = f 5

-- Exercise 6.3-i
-- Int -> (forall a. a -> a)
-- Rank == 1

-- Exercise 6.3-ii
-- (a -> b) -> (forall c. c -> a) -> b
-- (a -> b) -> ((forall c. c -> a) -> b)
-- Rank == 2

-- Exercise 6.3-iii
-- ((forall x. m x -> b (z m x))) -> b (z m a) -> m a
-- Rank == 3

cont :: a -> (forall r. (a -> r) -> r)
cont a = \cb -> cb a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f =
  let callback = id'
  in f callback

newtype Cont a = Cont
  {
    unCont :: forall r. (a -> r) -> r
  }

-- Exercise 6.4-i

instance Functor Cont where
  fmap f (Cont c) = Cont $ \c' -> c (c' . f)

-- Exercise 6.4-ii

instance Applicative Cont where
  -- pure  :: a -> f a
  pure a = Cont (\a' -> a' a)
  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) (Cont f) (Cont a) =
    Cont (\br -> f (\ab -> a $ br . ab))

-- Exercise 6.4-iii

instance Monad Cont where
  return = pure
  Cont m >>= f =
    Cont (\c -> m $ \a -> unCont (f a) c)

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimeStamp :: (Int -> r) -> r
withTimeStamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseString :: String
releaseString =
  withVersionNumber $ \version ->
    withTimeStamp $ \date ->
      withOS $ \os ->
        os ++ "-" ++ show version ++ "-" ++ show date

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimeStamp
  os <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date
