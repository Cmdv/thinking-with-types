{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}


module Ch02.Chapter02 where


-- Exercise 2.1.3-i
-- :k Show
-- Show :: TYPE -> Constraint

-- Exercise 2.1.3-ii
-- :k Functor
-- Functor :: (TYPE -> TYPE) -> Constraint


-- Exercise 2.1.3-iii
-- :k Monad
-- Monad :: (TYPE -> TYPE) -> Constraint


-- Exercise 2.1.3-iv
-- :k MonadTrans
-- MonadTrans :: ((TYPE -> TYPE) -> TYPE -> TYPE) -> Constraint

or' :: Bool -> Bool -> Bool
or' True _  = True
or' False y = y

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

-- Exersise 2.4-i

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True
