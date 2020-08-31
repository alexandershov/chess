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
findBestMove position@Position{P.sideToMove} = 
    findBestMoveAtDepth sideToMove 0 position


findBestMoveAtDepth :: Color -> Int -> Position -> Move
findBestMoveAtDepth maximizingPlayer depth position =
    getMove $ head scoredMoves
    where scoredMoves = getNextScoredMoves maximizingPlayer depth position


miniMaxEval :: Color -> Int -> Position -> Int
miniMaxEval maximizingPlayer depth position
    | depth == maxDepth = colorize maximizingPlayer (eval position)
    | moves == [] = colorize maximizingPlayer (eval position)
    | otherwise = head $ map getScore $ getNextScoredMoves maximizingPlayer depth position
    where moves = legalMoves position


getNextScoredMoves :: Color -> Int -> Position -> [(Int, Move)]
getNextScoredMoves maximizingPlayer depth position =
    applyMiniMax depth scoredMoves
    where nextPositionsWithMoves = [ ((makeUnchecked position move), move) | move <- moves ]
          moves = legalMoves position
          scoredMoves = [ ((miniMaxEval maximizingPlayer (depth + 1) p), m) | (p, m) <- nextPositionsWithMoves ]


applyMiniMax :: Int -> [(Int, Move)] -> [(Int, Move)]
applyMiniMax depth scoredMoves
    | even depth = sortOn scoreForMaximizing scoredMoves
    | otherwise = sortOn scoreForMinimizing scoredMoves


scoreForMaximizing :: (Int, Move) -> Int
scoreForMaximizing (score, _) = -score

scoreForMinimizing :: (Int, Move) -> Int
scoreForMinimizing (score, _) = score

getMove :: (Int, Move) -> Move
getMove = snd

getScore :: (Int, Move) -> Int
getScore = fst

colorize :: Color -> Int -> Int
colorize White evaluation = evaluation
colorize Black evaluation = -evaluation
