module Game
    ( Board
    , GameResult(..)
    , Move
    , checkBoard
    , getMove
    , move
    , newBoard
    , takeTurn
    ) where

import Data.Bifunctor (first, second)
import Data.Bits
import Data.Bool (bool)
import Data.List (find, foldl')
import Data.Maybe (fromJust)
import Data.Word (Word8, Word64)

newtype Board = B (Word64, Word64, Bool)
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
checkBoard = undefined

getMove :: Move -> Int
getMove (M x) = fromEnum x

move :: Int -> Maybe Move
move x
    | minMove <= x && x <= maxMove = Just . M . toEnum $ x
    | otherwise = Nothing

newBoard :: Board
newBoard = B (0, 0, False)

takeTurn :: Board -> Move -> Maybe Board
takeTurn (B (p1, p2, p)) (M c) = B . uncurry (,, not p) . flip f (p1, p2) . (.|.) . bitIn <$> row
  where
    bitIn r = bit $ 7 * r + fromEnum c
    f = bool first second p
    row = fromEnum <$> find (\r -> (p1 .|. p2) .&. bitIn r == 0) [0..5]
