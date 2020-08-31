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
    getMove $ head scoredMoves
    where moves = legalMoves position
          scores = getNextScores maximizingPlayer depth pickers moves position
          scoredMoves = sortOn bestScoreFirst $ zip scores moves
          pickers = cycle [minimum, maximum]


miniMaxEval :: Color -> Int -> Pickers -> Position -> Int
miniMaxEval maximizingPlayer depth pickers position
    | depth == maxDepth = colorize maximizingPlayer (eval position)
    | moves == [] = colorize maximizingPlayer (eval position)
    | otherwise = picker $ getNextScores maximizingPlayer depth nextPickers moves position
    where moves = legalMoves position
          picker:nextPickers = pickers


getNextScores :: Color -> Int -> Pickers -> [Move] -> Position -> [Int]
getNextScores maximizingPlayer depth pickers moves position =
    [ (miniMaxEval maximizingPlayer (depth + 1) pickers p) | p <- nextPositions ]
    where nextPositions = [ makeUnchecked position move | move <- moves ]


bestScoreFirst :: (Int, Move) -> Int
bestScoreFirst (score, _) = -score

getMove :: (Int, Move) -> Move
getMove = snd

colorize :: Color -> Int -> Int
colorize White evaluation = evaluation
colorize Black evaluation = -evaluation
