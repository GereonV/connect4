import AI
import Game
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

doIO :: Game -> IO (Maybe Game)
doIO g = print (getBoard g) >> maybe (Just <$> go) ((Nothing <$) . printR) (getResult g)
  where
    printR r = putStrLn $ case r of
        P1Won -> "Player 1 won"
        Drawn -> "Game drawn"
        P2Won -> "Player 2 won"
    go = do
        print . map fst $ possibleMoves g
        putStr "Choose column: "
        hFlush stdout
        m <- getLine
        ng <- maybe go return $ readMaybe m >>= move . (7 -) >>= takeTurn g
        putStrLn ""
        return ng

main :: IO ()
main = go newGame
  where go g = doIO g >>= return () `maybe` go
