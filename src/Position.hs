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
allMoves position = 
    [ move | 
     (piece, square) <- piecesInSquares position,
     move <- pieceMoves position piece square
    ]


piecesInSquares :: Position -> [(Piece, Squares)]
piecesInSquares Position board _ =
    [ (piece, square) | (square, Just piece) <- assocs board ]


pieceMoves :: Position -> Piece -> Square -> [Move]
pieceMoves (Position board _) piece square = []

type Direction = (Int, Int)
type Range = Int

data Movement = Movement [Direction] Range

getMovement :: Piece -> Movement
getMovement (Pawn _) = error "TODO: implement pawn movement"
getMovement (Knight _) = Movement jumps 1
getMovement (Bishop _) = Movement diagonals boardSize
getMovement (Rook _) = Movement straightLines boardSize
getMovement (Queen _) = Movement (diagonals ++ straightLines) boardSize
getMovement (King _) = Movement (diagonals ++ straightLines) 1