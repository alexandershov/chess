{-# LANGUAGE NamedFieldPuns #-}

module Engine where

import Data.Sort
import Eval hiding (colorize)
import Moves
import Pieces
import Position hiding (sideToMove)
import Position as P


findBestMove :: Position -> Move
findBestMove position@Position{P.sideToMove} = 
    getMove $ head scoredMoves
    where moves = legalMoves position
          nextPositions = [ makeUnchecked position move | move <- moves ]
          scores = [ colorize sideToMove (eval p) | p <- nextPositions ]
          scoredMoves = sortOn bestScoreFirst $ zip scores moves


bestScoreFirst :: (Int, Move) -> Int
bestScoreFirst (score, _) = -score

getMove :: (Int, Move) -> Move
getMove = snd

colorize :: Color -> Int -> Int
colorize White evaluation = evaluation
colorize Black evaluation = -evaluation
