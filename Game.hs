module Game
    ( Board
    , Game
    , GameResult(..)
    , Move
    , checkBoard
    , getBoard
    , getMove
    , getResult
    , isP2Turn
    , move
    , newGame
    , takeTurn
    ) where

import Data.Bifunctor (first, second)
import Data.Bits ((.<<.), (.&.), (.|.), bit)
import Data.Bool (bool)
import Data.List (find, foldl')
import Data.Maybe (fromJust, isNothing)
import Data.Word (Word8, Word64)

newtype Board = B (Word64, Word64)
data Game = G
    { getBoard  :: Board
    , getResult :: Maybe GameResult
    , isP2Turn  :: Bool }
data GameResult = P1Won | Drawn | P2Won
  deriving (Bounded, Eq, Ord, Read, Show)
newtype Move = M Word8
  deriving (Eq, Read, Show)

minMove, maxMove :: Int
minMove = 0
maxMove = 6

instance Bounded Move where
    minBound = toEnum minMove
    maxBound = toEnum maxMove

instance Enum GameResult where
    toEnum (-1) = P1Won
    toEnum 0    = Drawn
    toEnum 1    = P2Won

    fromEnum gr = case gr of
        P1Won -> -1
        Drawn ->  0
        P2Won ->  1

    enumFrom = flip enumFromTo maxBound

    enumFromThen x y = enumFromThenTo x y bound
      where
        bound
            | y >= x = maxBound
            | otherwise = minBound

instance Enum Move where
    toEnum = fromJust . move
    fromEnum (M x) = fromEnum x
    enumFrom = flip enumFromTo maxBound

    enumFromThen x y = enumFromThenTo x y bound
      where
        bound
            | fromEnum y >= fromEnum x = maxBound
            | otherwise = minBound

checkBoard :: Board -> Maybe GameResult
checkBoard (B (p1, p2))
    | p1 .|. p2 == bit (6 * 7) - 1 = Just Drawn
    | won p1 = Just P1Won
    | won p2 = Just P2Won
    | otherwise = Nothing
  where
    bitwise t = foldl' (.|.) 0 $ bit . t <$> [0..3]
    col = [shift row col colMask | col <- [0..6], row <- [0..2]]
    colMask = bitwise (7 *)
    diag = concat [shift row col <$> [diagMask1, diagMask2] | col <- [0..3], row <- [0..2]]
    diagMask1 = bitwise (8 *)
    diagMask2 = bitwise $ (3 +) . (6 *)
    row = [shift row col rowMask | col <- [0..3], row <- [0..5]]
    rowMask = bitwise id
    shift r c = (.<<. (7 * r + c))
    won p = any ((==) <*> (p .&.)) $ col ++ row ++ diag

getMove :: Move -> Int
getMove (M x) = fromEnum x

move :: Int -> Maybe Move
move x
    | minMove <= x && x <= maxMove = Just . M . toEnum $ x
    | otherwise = Nothing

newGame :: Game
newGame = G (curry B 0 0) Nothing False

takeTurn :: Game -> Move -> Maybe Game
takeTurn (G (B ps) r p) (M c)
    | isNothing r = ($ not p) . (G <*> checkBoard) . B . flip f ps . (.|.) . bitIn <$> row
    | otherwise = Nothing
  where
    bitIn r = bit $ 7 * r + fromEnum c
    f = bool first second p
    row = fromEnum <$> find (\r -> uncurry (.|.) ps .&. bitIn r == 0) [0..5]
