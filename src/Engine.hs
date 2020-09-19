{-# LANGUAGE NamedFieldPuns #-}

module Engine where

import Data.Sort
import Eval hiding (colorize)
import Moves
import Pieces
import Position hiding (sideToMove)
import Position as P


maxDepth :: Int
maxDepth = 5
forcingDepth :: Int
forcingDepth = 3

findBestMove :: Position -> Move
findBestMove position =
    getMove $ findBestScoredMove position


findBestScoredMove :: Position -> (Int, Move)
findBestScoredMove position@Position{P.sideToMove} = 
    findBestScoredMoveAtDepth sideToMove 0 position


findBestScoredMoveAtDepth :: Color -> Int -> Position -> (Int, Move)
findBestScoredMoveAtDepth maximizingPlayer depth position =
    head scoredMoves
    where scoredMoves = getNextScoredMoves (initAlphaBeta depth) maximizingPlayer depth position


miniMaxEval :: Int -> Color -> Int -> Position -> Int
miniMaxEval alphaBeta maximizingPlayer depth position
    | isLeaf = position `evalFor` maximizingPlayer
    | otherwise = head $ map getScore $ getNextScoredMoves alphaBeta maximizingPlayer depth position
    where isLeaf = depth == maxDepth || getCandidateMoves depth position == []


getNextScoredMoves :: Int -> Color -> Int -> Position -> [(Int, Move)]
getNextScoredMoves parentAlphaBeta maximizingPlayer depth position =
    applyMiniMax depth scoredMoves
    where nextPositionsWithMoves = [ ((makeUnchecked position move), move) | move <- moves ]
          moves = getCandidateMoves depth position
          scoredMoves = alphaBetaReduce parentAlphaBeta maximizingPlayer 
                                        (initAlphaBeta depth) depth [] nextPositionsWithMoves


getCandidateMoves :: Int -> Position -> [Move]
getCandidateMoves depth position
    | depth >= maxDepth - forcingDepth = forcingMoves position
    | otherwise = legalMoves position
    

applyMiniMax :: Int -> [(Int, Move)] -> [(Int, Move)]
applyMiniMax depth scoredMoves
    | isMaximizing depth = sortOn maxFirst scoredMoves
    | otherwise = sortOn minFirst scoredMoves


alphaBetaReduce :: Int -> Color -> Int -> Int -> [(Int, Move)] -> [(Position, Move)] -> [(Int, Move)]

alphaBetaReduce _ _ _ _ acc [] = reverse acc
alphaBetaReduce parent maximizingPlayer current depth acc ((position, move):nextPositionsWithMoves)
   | isMinimizing depth && current < parent = reverse acc
   | otherwise = alphaBetaReduce parent maximizingPlayer (combineAlphaBeta depth current score)
                                 depth ((score, move):acc) nextPositionsWithMoves
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


maxFirst :: (Int, Move) -> Int
maxFirst (score, _) = -score


minFirst :: (Int, Move) -> Int
minFirst (score, _) = score


getMove :: (Int, Move) -> Move
getMove = snd


getScore :: (Int, Move) -> Int
getScore = fst


colorize :: Color -> Int -> Int
colorize White evaluation = evaluation
colorize Black evaluation = -evaluation


evalFor :: Position -> Color -> Int
position `evalFor` color = 
    colorize color (eval position) 