-- Tic Tac Toe, because why not

{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances
    #-}

module TicTacToe where

import Data.Function (on)
import Data.Maybe (maybeToList)
import qualified Data.Map.Strict as M

import SuperSimpleFormatter

-- Type-safe counting FTW
data I3 = One | Two | Three deriving (Show, Read, Eq, Ord, Enum)

type Pos = (I3, I3)

data Player = X | O deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

type Board = M.Map Pos Player

clearBoard :: Board
clearBoard = M.empty

occupiedFields :: Board -> [Pos]
occupiedFields = M.keys

setField :: Pos -> Player -> Board -> Board
setField = M.insert

isFull :: Board -> Bool
isFull = (== 9) . M.size

isFirstTurn :: Board -> Bool
isFirstTurn = (<= 1) . M.size

isEmpty :: Board -> Bool
isEmpty = M.null

-- returns either a singleton list or []
joinPlayers :: [Player] -> [Player]
joinPlayers [x, y, z] | x == y && y == z = [x]
joinPlayers _ = []

getCellState :: Board -> Pos -> Maybe Player
getCellState = flip M.lookup

gridLines :: [[Pos]]
gridLines =  [zip (repeat x) [One ..] | x <- [One ..]]
      ++ [zip [One ..] (repeat y) | y <- [One ..]]
      ++ [zip [One ..] [One ..]]
      ++ [zip [One ..] [Three, Two, One]]

possibleCoords :: [Pos]
possibleCoords = [(x, y) | x <- [One ..], y <- [One ..]]

checkWin :: Board -> Maybe Player
checkWin b = case gridLines >>= joinPlayers . concatMap (maybeToList . getCellState b) of
                [] -> Nothing
                s:_ -> Just s

instance {-# OVERLAPS #-} Ord (Maybe Player) where
    -- Just X < Nothing < Just O
    p1 <= p2 = case p1 of
        Just X -> True
        Nothing -> p2 /= Just X
        Just O -> p2 == Just O