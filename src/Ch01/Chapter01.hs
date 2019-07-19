module Ch01.Chapter01 where

-- Exersise 1.2-i

-- determine the cardinality of:
-- Either Bool (Bool, Maybe Bool) -> Bool
--        2    + (2 x (1 + 2))    √  2
--        2    + (     6     )    √  2
--        8 x 8 = 256


-- Example Tic-Tac-Toe

data TicTacToe a = TicTacToe
  { topLeft   :: a
  , topCenter :: a
  , topRight  :: a
  , midLeft   :: a
  , midCenter :: a
  , midRight  :: a
  , botLeft   :: a
  , botCenter :: a
  , botRight  :: a
  }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard =

  TicTacToe
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing

data Three = One | Two | Three
 deriving (Eq, Ord, Enum, Bounded)

data TicTacToe2 a = TicTacToe2
  { board :: Three -> Three -> a
  }

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 = TicTacToe2 $ const $ const Nothing

-- Playing arround a little bit more
data XO = X | O

type Board = Three -> Three -> Maybe XO

updateBoard :: XO -> Three -> Three -> Board -> Board
updateBoard p x y b x' y' =

    if x == x' && y == y'
    then Just p
    else b x' y'


-- Exersise 1.4-i

-- (b -> c -> a) -> (b, c) -> a
-- ((b, c) -> a) -> b -> c -> a

-- uncurry
funcCur :: (b -> c -> a) -> (b, c) -> a
funcCur f (b, c) = f b c

-- curry
funcCur' :: ((b, c) -> a) -> b -> c -> a
funcCur' f b c = f (b, c)

-- these are like the `to . from`

-- Exersise 1.2-ii
-- ((b -> a) , (c -> a)) == (Either b c) -> a

xIIto
  :: (b -> a)
  -> (c -> a)
  -> Either b c
  -> a
xIIto f _ (Left b)  = f b
xIIto _ g (Right y) = g y

xIIfrom
  :: (Either b c -> a)
  -> (b -> a , c -> a)
xIIfrom f = (f . Left , f . Right)


-- Exersise 1.4-iii
-- c -> (a, b) -> (c -> a, c -> b)

xIIIto :: (c -> (a, b)) -> (c -> a, c -> b)
xIIIto f = (fst . f, snd . f)

xIIIfrom :: (c -> a) -> (c -> b) -> c -> (a, b)
xIIIfrom f g c = (f c, g c)
