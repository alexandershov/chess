import qualified Data.Array as Array

data Color = Black | White
data Piece = 
    Pawn Color | 
    Knight Color | 
    Bishop Color |
    Rook Color | 
    Queen Color | 
    King Color

type Square = (Int, Int)
type Board = Array Square (Maybe Piece)

data Position = Position { board::Board, }