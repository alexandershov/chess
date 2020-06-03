{-# LANGUAGE NamedFieldPuns #-}

module Eval where

import Data.Array (elems)

import Pieces
import Position hiding (board)
import qualified Position as P

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
eval Position{P.board} = 
    sum [maybe 0 valueOf piece | piece <- elems board]
