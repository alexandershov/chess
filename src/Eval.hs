{-# LANGUAGE NamedFieldPuns #-}

module Eval where

import Data.Array (elems)

import Moves
import Pieces
import Position hiding (board, sideToMove)
import qualified Position as P

moveCost :: Color -> Int
moveCost color = colorize 10 color


valueOf :: Piece -> Int
valueOf (Queen color) = colorize 900 color
valueOf (Rook color) = colorize 500 color
valueOf (Bishop color) = colorize 350 color
valueOf (Knight color) = colorize 300 color
valueOf (Pawn color) = colorize 100 color
valueOf (King color) = colorize 0 color

colorize :: Int -> Color -> Int
colorize value White = value
colorize value Black = -value

eval :: Position -> Int
eval position =
    case moves of
        [] -> terminalEval position
        _ -> materialEval position + positionalEval position + positionalEval rivalPosition
    where moves = legalMoves position
          rivalPosition = makeNullMove position


materialEval :: Position -> Int
materialEval Position{P.board} = 
    sum [maybe 0 valueOf piece | piece <- elems board]


positionalEval :: Position -> Int
positionalEval position@Position{P.sideToMove} =
    numMoves * moveCost sideToMove
    where numMoves = length $ legalMoves position


makeNullMove :: Position -> Position
makeNullMove position@Position{P.sideToMove} = 
    position{P.sideToMove=rival sideToMove, P.enPassant=Nothing}


terminalEval :: Position -> Int
terminalEval position@Position{P.sideToMove} = 
    if sideToMove `isUnderCheckIn` makeNullMove position 
        then evalMateOf sideToMove
        else 0

evalMateOf :: Color -> Int
evalMateOf White = minBound
evalMateOf Black = maxBound
