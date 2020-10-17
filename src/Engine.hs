{-# LANGUAGE NamedFieldPuns #-}

module Engine where

import Data.Sort
import Eval hiding (colorize)
import Moves
import Pieces
import Position hiding (sideToMove)
import Position as P


maxDepth :: Int
maxDepth = 6
forcingDepth :: Int
forcingDepth = 4

findBestMove :: Position -> Move
findBestMove position =
    getMove $ head $ findBestScoredMoves position


findBestScoredMoves :: Position -> [(Int, Move)]
findBestScoredMoves position@Position{P.sideToMove} = 
    findBestScoredMovesAtDepth sideToMove 0 position

findBestScoredMovesAtDepth :: Color -> Int -> Position -> [(Int, Move)]
findBestScoredMovesAtDepth maximizingPlayer depth position =
    scoredMoves
    where scoredMoves = getNextScoredMoves (initAlphaBeta depth) maximizingPlayer depth False position


miniMaxEval :: Int -> Color -> Int -> Bool -> Position -> Int
miniMaxEval alphaBeta maximizingPlayer depth isForcingLine position
    | isLeaf = position `evalFor` maximizingPlayer
    | otherwise = head $ map getScore $ getNextScoredMoves alphaBeta maximizingPlayer depth isForcingLine position
    where isLeaf = isAtMaxDepth depth || getCandidateMoves depth isForcingLine position == []


getNextScoredMoves :: Int -> Color -> Int -> Bool -> Position -> [(Int, Move)]
getNextScoredMoves parentAlphaBeta maximizingPlayer depth isForcingLine position =
    applyMiniMax depth scoredMoves
    where nextPositionsWithMoves = [ ((makeUnchecked position move), (isForcingMove position move), move) | move <- moves ]
          moves = getCandidateMoves depth isForcingLine position
          scoredMoves = alphaBetaReduce parentAlphaBeta maximizingPlayer 
                                        (initAlphaBeta depth) depth [] isForcingLine nextPositionsWithMoves


getCandidateMoves :: Int -> Bool -> Position -> [Move]
getCandidateMoves depth isForcingLine position
    | isAtForcingDepth depth && isForcingLine = forcingMoves position
    | isAtForcingDepth depth = []
    | otherwise = legalMoves position


isAtMaxDepth :: Int -> Bool
isAtMaxDepth depth =
    depth == maxDepth

isAtForcingDepth :: Int -> Bool
isAtForcingDepth depth = 
    depth >= maxDepth - forcingDepth
    

applyMiniMax :: Int -> [(Int, Move)] -> [(Int, Move)]
applyMiniMax depth scoredMoves
    | isMaximizing depth = sortOn maxFirst scoredMoves
    | otherwise = sortOn minFirst scoredMoves


alphaBetaReduce :: Int -> Color -> Int -> Int -> [(Int, Move)] -> Bool -> [(Position, Bool, Move)] -> [(Int, Move)]

alphaBetaReduce _ _ _ _ acc _ [] = reverse acc
alphaBetaReduce parent maximizingPlayer current depth acc isForcingLine ((position, isForcingMove', move):nextPositionsWithMoves)
   | isMinimizing depth && current < parent = reverse acc
   | otherwise = alphaBetaReduce parent maximizingPlayer (combineAlphaBeta depth current score)
                                 depth ((score, move):acc) isForcingLine nextPositionsWithMoves
        where score = (miniMaxEval current maximizingPlayer (depth + 1) isForcingMove' position)


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