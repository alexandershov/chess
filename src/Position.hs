module Position where

import Data.Array (Array, (!), listArray)
import Data.Maybe (isJust)

boardSize :: Int
boardSize = 8

data Color = Black | White
data Piece = 
    Pawn Color | 
    Knight Color | 
    Bishop Color |
    Rook Color | 
    Queen Color | 
    King Color

type Square = (Int, Int)
showSquare :: Square -> String
showSquare (x, y) = 
    ("abcdefgh" !! (x - 1)):(show y)

type Board = Array Square (Maybe Piece)

data Position = Position { getBoard :: Board, getSideToMove :: Color }
data Move = Move { from :: Square, to :: Square } deriving (Eq)

instance Show Move where
    show move = showSquare (from move) ++ showSquare (to move)

emptyBoard :: Board
emptyBoard = listArray ((1, 1), (8, 8)) $ replicate (boardSize * boardSize) Nothing

isOccupied :: Square -> Board -> Bool
square `isOccupied` board = isJust $ board ! square

allMoves :: Position -> [Move]
allMoves _ = []