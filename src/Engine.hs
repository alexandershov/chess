{-# LANGUAGE NamedFieldPuns #-}

module Engine where

import Data.Sort
import Eval hiding (colorize)
import Moves
import Pieces
import Position hiding (sideToMove)
import Position as P

data Node = Node {maximizingPlayer :: Color,
                  depth :: Int,
                  position :: Position,
                  isForcingLine :: Bool}

maxDepth :: Int
maxDepth = 6
forcingDepth :: Int
forcingDepth = 4

findBestMove :: Position -> Move
findBestMove position =
    getMove $ head $ findBestScoredMoves position


initialNode :: Position -> Node
initialNode position@Position{P.sideToMove} = Node {
    maximizingPlayer = sideToMove, depth = 0, position = position, isForcingLine = False}

findBestScoredMoves :: Position -> [(Int, Move)]
findBestScoredMoves position =
    findBestScoredMovesAtNode $ initialNode position

findBestScoredMovesAtNode :: Node -> [(Int, Move)]
findBestScoredMovesAtNode node =
    scoredMoves
    where scoredMoves = getNextScoredMoves (initialAlphaBeta node) node


miniMaxEval :: Int -> Node -> Int
miniMaxEval alphaBeta node
    | isLeaf = (position node) `evalFor` (maximizingPlayer node)
    | otherwise = head $ map getScore $ getNextScoredMoves alphaBeta node
    where isLeaf = isAtMaxDepth node || getCandidateMoves node == []


getNextScoredMoves :: Int -> Node -> [(Int, Move)]
getNextScoredMoves parentAlphaBeta node@Node{position} =
    applyMiniMax node scoredMoves
    where nextPositionsWithMoves = [ ((makeUnchecked position move), (isForcingMove position move), move) | move <- moves ]
          moves = getCandidateMoves node
          scoredMoves = alphaBetaReduce parentAlphaBeta (initialAlphaBeta node) node [] nextPositionsWithMoves


getCandidateMoves :: Node -> [Move]
getCandidateMoves node@Node{isForcingLine, position}
    | isAtForcingDepth node && isForcingLine = forcingMoves position
    | isAtForcingDepth node = []
    | otherwise = legalMoves position


isAtMaxDepth :: Node -> Bool
isAtMaxDepth Node{depth} =
    depth == maxDepth

isAtForcingDepth :: Node -> Bool
isAtForcingDepth Node{depth} = 
    depth >= maxDepth - forcingDepth
    

applyMiniMax :: Node -> [(Int, Move)] -> [(Int, Move)]
applyMiniMax node scoredMoves
    | isMaximizing node = sortOn maxFirst scoredMoves
    | otherwise = sortOn minFirst scoredMoves


alphaBetaReduce :: Int -> Int -> Node -> [(Int, Move)] -> [(Position, Bool, Move)] -> [(Int, Move)]

alphaBetaReduce _ _ _ acc [] = reverse acc
alphaBetaReduce parent current node acc ((position, isForcingMove', move):nextPositionsWithMoves)
   | isMinimizing node && current < parent = reverse acc
   | otherwise = alphaBetaReduce parent (combineAlphaBeta node current score) node ((score, move):acc) nextPositionsWithMoves
        where score = (miniMaxEval current (nextNode node position isForcingMove'))


nextNode :: Node -> Position -> Bool -> Node
nextNode node@Node{depth} nextPosition isForcingMove' = 
    node{depth=depth + 1, position=nextPosition, isForcingLine=isForcingMove'}

combineAlphaBeta :: Node -> Int -> Int -> Int
combineAlphaBeta node current score
    | isMaximizing node = max current score
    | otherwise = min current score


initialAlphaBeta :: Node -> Int
initialAlphaBeta node
    | isMaximizing node = evalMateOf White
    | otherwise = evalMateOf Black


isMaximizing :: Node -> Bool
isMaximizing Node{depth} = even depth


isMinimizing :: Node -> Bool
isMinimizing node = not $ isMaximizing node


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