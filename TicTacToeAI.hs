module TicTacToeAI where

import GHC.Exts

import Control.Arrow  -- BECAUSE I CAN

import Data.List

import TicTacToe

optimalMove :: Player -> Board -> Pos
optimalMove p b = if isFirstTurn b -- Special-case for opening moves, otherwise they take too long
                  then case p of
                        X -> (One, One)
                        O -> case getCellState b (Two, Two) of
                            Nothing -> (Two, Two)
                            Just X  -> (One, One)
                  else case wins of
                        [] -> head draws
                        w:_ -> w
                  where fs :: [(Pos, [([Pos], Board)])]
                        fs = groupFutures $ futures p b
                        fsWithResults :: [(Maybe Player, (Pos, [([Pos], Board)]))]
                        fsWithResults = map ((snd >>> map snd >>> worstResult p) &&& id) fs
                        wins :: [Pos]
                        wins  = filter (fst >>> (== Just p))  >>> map (snd >>> fst) $ fsWithResults
                        draws :: [Pos]
                        draws = filter (fst >>> (== Nothing)) >>> map (snd >>> fst) $ fsWithResults

futures :: Player -> Board -> [([Pos], Board)]
futures p b = if (checkWin b) /= Nothing
              then [([], b)]
              else case possibleCoords \\ occupiedFields b of
                        [] -> [([], b)]
                        freefields -> do field <- freefields
                                         let newBoard = setField field p b
                                         map ((field :) *** id)
                                             (futures (otherPlayer p) newBoard)

groupFutures :: [([Pos], Board)] -> [(Pos, [([Pos], Board)])]
groupFutures = groupWith (fst >>> head) >>> map ((head >>> fst >>> head) &&& id)


-- I think this is a bit wonky. TODO fix.
worstResult :: Player -> [Board] -> Maybe Player
worstResult p = map checkWin >>> (case p of O -> minimum; X -> maximum)
