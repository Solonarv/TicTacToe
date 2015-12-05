module TicTactToeLocalPvP where

import Data.Function (on)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import TicTacToe
import TicTacToeIO

main = do (winner, (_, finalBoard)) <- runStateT runUntilWin (X, clearBoard)
          putStrLn $ announceResult winner
          putStrLn "The final board state is:"
          putStrLn $ pprintBoard finalBoard


runUntilWin :: GameStateIO (Maybe Player)
runUntilWin = do p <- nextPlayer
                 b <- theBoard
                 fmap pprintBoard theBoard >>= lift . putStr
                 lift $ putStrLn $ show p ++ ", make your move! (format: 'An' where A is the column and n is the row in which you wish to place an " ++ show p ++ "."
                 move <- getMove $ occupiedFields b -- this does not terminate until a valid move is input
                 let newBoard = setField move p b
                 case checkWin newBoard of
                     Nothing -> if isFull b
                                then return Nothing
                                else put (otherPlayer p, newBoard) >> runUntilWin
                     Just x  -> return $ Just x