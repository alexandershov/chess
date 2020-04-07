import qualified Data.Array as Array

boardSize :: Int = 8

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

data Position = Position { board :: Board, sideToMove :: Color }
data Move = Move { from :: Square, to :: Square }

