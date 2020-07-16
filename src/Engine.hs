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


findBestMove :: Position -> Move
findBestMove position = findBestMoveAtDepth 0 position


findBestMoveAtDepth :: Int -> Position -> Move
findBestMoveAtDepth depth position =
    getMove $ head scoredMoves
    where moves = legalMoves position
          scores = getNextScores depth moves position
          scoredMoves = sortOn bestScoreFirst $ zip scores moves


negaMaxEval :: Int -> Position -> Int
negaMaxEval depth position@Position{P.sideToMove}
    | depth == maxDepth = eval position
    | moves == [] = eval position
    | otherwise = colorize sideToMove (maximum $ getNextScores depth moves position)
    where moves = legalMoves position


getNextScores :: Int -> [Move] -> Position -> [Int]
getNextScores depth moves position@Position{P.sideToMove} =
    [ colorize sideToMove (negaMaxEval (depth + 1) p) | p <- nextPositions ]
    where nextPositions = [ makeUnchecked position move | move <- moves ]


bestScoreFirst :: (Int, Move) -> Int
bestScoreFirst (score, _) = -score

getMove :: (Int, Move) -> Move
getMove = snd

colorize :: Color -> Int -> Int
colorize White evaluation = evaluation
colorize Black evaluation = -evaluation
