module TicTacToePvC where

import Data.Char
import Data.Function (on)
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import TicTacToe
import TicTacToeIO
import TicTacToeAI

main = do humanside <- promptHumanSide
          (winner, (_, finalBoard)) <- runStateT (runUntilWin humanside) (X, clearBoard)
          putStrLn $ announceResult winner
          putStrLn "The final board state is:"
          putStrLn $ pprintBoard finalBoard

promptHumanSide :: IO Player
promptHumanSide = do putStr "Player, choose a side: crosses (X) or circles (O): "
                     promptSide'
    where promptSide' = do l <- getLine
                           case map toUpper l of
                               "O" -> return O
                               "X" -> return X
                               otherwise -> putStr "Invalid input. Please enter X or O: " >> promptSide'


runUntilWin :: Player -> GameStateIO (Maybe Player)
runUntilWin humanside = do p <- nextPlayer
                           b <- theBoard
                           fmap pprintBoard theBoard >>= lift . putStr
                           newBoard <- if p == humanside
                                       then do move <- getMove $ occupiedFields b
                                               return $ setField move p b
                                       else return $ setField (optimalMove p b) p b
                           case checkWin newBoard of
                               Nothing -> if isFull b
                                          then modify (second $ const newBoard) >> return Nothing
                                          else put (otherPlayer p, newBoard) >> runUntilWin humanside
                               Just x  -> modify (second $ const newBoard) >> return (Just x)