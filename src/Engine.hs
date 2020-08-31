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
    where scoredMoves = getNextScoredMoves (evalMateOf White) maximizingPlayer depth position


miniMaxEval :: Int -> Color -> Int -> Position -> Int
miniMaxEval alphaBeta maximizingPlayer depth position
    | depth == maxDepth = colorize maximizingPlayer (eval position)
    | moves == [] = colorize maximizingPlayer (eval position)
    | otherwise = head $ map getScore $ getNextScoredMoves alphaBeta maximizingPlayer depth position
    where moves = legalMoves position


getNextScoredMoves :: Int -> Color -> Int -> Position -> [(Int, Move)]
getNextScoredMoves parentAlphaBeta maximizingPlayer depth position =
    applyMiniMax depth $ alphaBetaReduce parentAlphaBeta maximizingPlayer (initAlphaBeta depth) depth [] nextPositionsWithMoves
    where nextPositionsWithMoves = [ ((makeUnchecked position move), move) | move <- moves ]
          moves = legalMoves position


applyMiniMax :: Int -> [(Int, Move)] -> [(Int, Move)]
applyMiniMax depth scoredMoves
    | isMaximizing depth = sortOn scoreForMaximizing scoredMoves
    | otherwise = sortOn scoreForMinimizing scoredMoves


alphaBetaReduce :: Int -> Color -> Int -> Int -> [(Int, Move)] -> [(Position, Move)] -> [(Int, Move)]

alphaBetaReduce _ _ _ _ acc [] = acc
alphaBetaReduce parent maximizingPlayer current depth acc ((position, move):nextPositionsWithMoves)
   | isMinimizing depth && current < parent = acc
   | otherwise = alphaBetaReduce parent maximizingPlayer (combineAlphaBeta depth current score) depth ((score, move):acc) nextPositionsWithMoves
        where score = (miniMaxEval current maximizingPlayer (depth + 1) position)


combineAlphaBeta :: Int -> Int -> Int -> Int
combineAlphaBeta depth current score
    | isMaximizing depth = max current score
    | otherwise = min current score

initAlphaBeta :: Int -> Int
initAlphaBeta depth
    | isMaximizing depth = evalMateOf White
    | otherwise = evalMateOf Black


isMaximizing :: Int -> Bool
isMaximizing depth = even depth

isMinimizing :: Int -> Bool
isMinimizing depth = not $ isMaximizing depth


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
