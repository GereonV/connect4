module AI (findBestMove, possibleMoves) where

import Data.Bool (bool)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Maybe (mapMaybe)
import Game

evalGame :: Game -> GameResult
evalGame g = case getResult g of
    Just r -> r
    Nothing -> undefined

findBestMove :: Game -> (Move, Game)
findBestMove = best <*> possibleMoves
  where best = ($ on compare $ evalGame . snd) . bool maximumBy minimumBy . isP1Turn

possibleMoves :: Game -> [(Move, Game)]
possibleMoves g =  mapMaybe (\m -> (m,) <$> takeTurn g m) moveOrder
  where moveOrder = sortBy (compare `on` abs . (3 -) . getMove) [minBound..]
