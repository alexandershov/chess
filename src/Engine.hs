{-# LANGUAGE NamedFieldPuns #-}

module Engine where

import Data.Sort
import Eval hiding (colorize)
import Moves
import Pieces
import Position hiding (sideToMove)
import Position as P


maxDepth :: Int
maxDepth = 2

type Pickers = [[Int] -> Int]


findBestMove :: Position -> Move
findBestMove position@Position{P.sideToMove} = 
    findBestMoveAtDepth sideToMove 0 position


findBestMoveAtDepth :: Color -> Int -> Position -> Move
findBestMoveAtDepth maximizingPlayer depth position =
    getMove $ head $ sortOn bestScoreFirst scoredMoves
    where scoredMoves = getNextScoredMoves maximizingPlayer depth pickers position
          pickers = cycle [minimum, maximum]


miniMaxEval :: Color -> Int -> Pickers -> Position -> Int
miniMaxEval maximizingPlayer depth pickers position
    | depth == maxDepth = colorize maximizingPlayer (eval position)
    | moves == [] = colorize maximizingPlayer (eval position)
    | otherwise = picker $ map getScore $ getNextScoredMoves maximizingPlayer depth nextPickers position
    where moves = legalMoves position
          picker:nextPickers = pickers


getNextScoredMoves :: Color -> Int -> Pickers -> Position -> [(Int, Move)]
getNextScoredMoves maximizingPlayer depth pickers position =
    [ ((miniMaxEval maximizingPlayer (depth + 1) pickers p), m) | (p, m) <- nextPositionsWithMoves ]
    where nextPositionsWithMoves = [ ((makeUnchecked position move), move) | move <- moves ]
          moves = legalMoves position


bestScoreFirst :: (Int, Move) -> Int
bestScoreFirst (score, _) = -score

getMove :: (Int, Move) -> Move
getMove = snd

getScore :: (Int, Move) -> Int
getScore = fst

colorize :: Color -> Int -> Int
colorize White evaluation = evaluation
colorize Black evaluation = -evaluation
