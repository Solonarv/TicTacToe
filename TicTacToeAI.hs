module TicTacToeAI where

import GHC.Exts

import Control.Arrow  -- BECAUSE I CAN

import Data.List

import TicTacToe

optimalMove :: Player -> Board -> Pos
optimalMove p b = case wins of
                        [] -> head draws
                        w:_ -> head w
                  where fs = groupFutures $ futures p b
                        fsWithResults = map ((worstResult . snd) &&& id) fs
                        wins  = map (fst . snd) $ filter (all ((== Just p) . fst)) fsWithResults
                        draws = map (fst . snd) $ filter (all ((/= Just (otherPlayer p)) . fst)) fsWithResults

futures :: Player -> Board -> [([Pos], Board)]
futures p b = if (checkWin b) /= Nothing
              then [([], b)]
              else case possibleCoords \\ occupiedFields b of
                        [] -> [([], b)]
                        freefields -> do field <- freefields
                                         let newBoard = setField field p b
                                         for (futures (otherPlayer p) newBoard)
                                             \(moves, finalBoard) -> (field : moves, finalBoard)

groupFutures :: [([Pos], Board)] -> [(Pos, [([Pos], Board)])]
groupFutures = map ((head . fst . head . head) &&& id) . groupWith (head . fst)

worstResult :: Player -> [([Pos], Board)] -> Maybe Player
worstResult p = flip foldl (Just $ otherPlayer p) (maxResult p) . map (checkWin . snd)


