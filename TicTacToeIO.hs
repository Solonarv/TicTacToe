module TicTacToeIO (
                    getMove,
                    parseMove,
                    announceResult,
                    pprintBoard,
                    showCellState,
                    GameStateIO,
                    nextPlayer,
                    theBoard
                    ) where

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import SuperSimpleFormatter
import TicTacToe

type GameStateIO = StateT (Player, Board) IO

-- not strictly necessary, but makes code more readable
nextPlayer :: GameStateIO Player
nextPlayer = gets fst
theBoard :: GameStateIO Board
theBoard = gets snd

getMove :: [Pos] -> GameStateIO Pos
getMove occupied = do 
    move <- lift getLine
    pm <- parseMove occupied move
    if pm `elem` occupied then retry occupied else return pm

parseMove :: [Pos] -> String -> GameStateIO Pos
parseMove occupied move | length move == 2
                        = let cc:rc:"" = move
                              coli = elemIndex (toUpper cc) "LCR"
                              rowi = elemIndex (toUpper rc) "123"
                          in if coli == Nothing || rowi == Nothing
                             then retry occupied
                             else return (toEnum $ fromJust coli, toEnum $ fromJust rowi)
                        | otherwise = retry occupied

retry :: [Pos] -> GameStateIO Pos
retry occupied = lift (putStrLn "Invalid format or field already occupied. Please try again.") >> getMove occupied


announceResult :: Maybe Player -> String
announceResult Nothing = "The game ended in a draw."
announceResult (Just X) = "Crosses win!"
announceResult (Just O) = "Circles win!"

pprintBoard :: Board -> String
pprintBoard board = unlines $ [ "   | L | C | R |   ",
                                "---+---+---+---+---",
                                " 1 | _ | _ | _ | 1 " % [showCellState $ getCellState board (x, One)   | x <- [One ..]],
                                "---+---+---+---+---",
                                " 2 | _ | _ | _ | 2 " % [showCellState $ getCellState board (x, Two)   | x <- [One ..]],
                                "---+---+---+---+---",
                                " 3 | _ | _ | _ | 3 " % [showCellState $ getCellState board (x, Three) | x <- [One ..]],
                                "---+---+---+---+---",
                                "   | L | C | R |   "]

showCellState :: Maybe Player -> String
showCellState Nothing = " "
showCellState (Just p) = show p