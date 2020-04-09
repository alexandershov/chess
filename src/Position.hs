module Position where

import Data.Array (assocs, Array, (!), listArray)
import Data.Maybe (isNothing)

boardSize :: Int
boardSize = 8

data Color = Black | White deriving (Eq, Show)
data Piece = 
    Pawn Color | 
    Knight Color | 
    Bishop Color |
    Rook Color | 
    Queen Color | 
    King Color


instance Show Piece where
    show (Pawn _) = "p"
    show (Knight _) = "N"
    show (Bishop _) = "B"
    show (Rook _) = "R"
    show (Queen _) = "Q"
    show (King _) = "K"

type Square = (Int, Int)
showSquare :: Square -> String
showSquare (x, y) = 
    ("abcdefgh" !! (x - 1)):(show y)

type Board = Array Square (Maybe Piece)

data Position = Position { getBoard :: Board, getSideToMove :: Color }
data Move = Move { from :: Square, to :: Square } deriving (Eq)

type Direction = (Int, Int)
type Range = Int

data Movement = Movement [Direction] Range

instance Show Move where
    show move = showSquare (from move) ++ showSquare (to move)

emptyBoard :: Board
emptyBoard = listArray ((1, 1), (8, 8)) $ replicate (boardSize * boardSize) Nothing

isOnBoard :: Square -> Bool
isOnBoard (x, y) = and $ map coordinateIsOnBoard [x, y]

coordinateIsOnBoard :: Int -> Bool
coordinateIsOnBoard x = (x >= 1) && (x <= boardSize)

isEmpty :: Board -> Square -> Bool
isEmpty board square = isNothing $ board ! square

allMoves :: Position -> [Move]
allMoves position = 
    [ move | 
     (piece, square) <- piecesInSquares position,
     move <- pieceMoves position piece square
    ]


piecesInSquares :: Position -> [(Piece, Square)]
piecesInSquares (Position board sideToMove) =
    [ (piece, square) | 
      (square, Just piece) <- assocs board, 
      getColor piece == sideToMove ]


pieceMoves :: Position -> Piece -> Square -> [Move]
pieceMoves (Position board sideToMove) piece square = 
    let destinations = getDestinations square (getMovement piece)
        candidates = takeWhileAndNext (isEmpty board) destinations
        tos = filter (notColoredAs board sideToMove) candidates in
           map (Move square) tos


notColoredAs :: Board -> Color -> Square -> Bool
notColoredAs board color square =
    case maybePiece of
        Nothing -> True
        Just piece -> (getColor piece) /= color
    where maybePiece = board ! square

takeWhileAndNext :: (a -> Bool) -> [a] -> [a]
takeWhileAndNext p xs = 
    good ++ take 1 bad
    where (good, bad) = span p xs


getDestinations :: Square -> Movement -> [Square]
getDestinations square (Movement directions range) =
    filter isOnBoard [ squareInDirection square d i | d <- directions, i <- [1..range] ]


mulDirection :: Direction -> Int -> Direction
mulDirection (x, y) n = (x * n, y * n)


addDelta :: Square -> Direction -> Square
addDelta (x, y) (dx, dy) = (x + dx, y + dy)

squareInDirection :: Square -> Direction -> Range -> Square
squareInDirection square direction range = 
    addDelta square $ mulDirection direction range


getMovement :: Piece -> Movement
getMovement (Pawn _) = error "TODO: implement pawn movement"
getMovement (Knight _) = Movement jumps 1
getMovement (Bishop _) = Movement diagonals boardSize
getMovement (Rook _) = Movement straightLines boardSize
getMovement (Queen _) = Movement (diagonals ++ straightLines) boardSize
getMovement (King _) = Movement (diagonals ++ straightLines) 1

getColor :: Piece -> Color
getColor (Pawn color) = color
getColor (Knight color) = color
getColor (Bishop color) = color
getColor (Rook color) = color
getColor (Queen color) = color
getColor (King color) = color

jumps :: [Direction]
jumps = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

diagonals :: [Direction]
diagonals = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

straightLines :: [Direction]
straightLines = [(1, 0), (0, 1), (-1, 0), (0, -1)]
