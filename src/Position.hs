{-# LANGUAGE OverloadedLabels #-}

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

type Square = (Int, Int)
showSquare :: Square -> String
showSquare (x, y) = 
    file ++ rank
    where file = [("abcdefgh" !! (x - 1))]
          rank = show y

type Board = Array Square (Maybe Piece)

data Position = Position Board Color
data Move = Move Square Square deriving (Eq)

type Direction = (Int, Int)
type Range = Int

data Movement = Movement [Direction] Range

instance Show Move where
    show (Move from to) 
        | isOnBoard from && isOnBoard to = showSquare from ++ showSquare to
        | otherwise = "MoveOutsideTheBoard " ++ (show from) ++ " -> " ++ (show to)

emptyBoard :: Board
emptyBoard = listArray (a1, h8) $ repeat Nothing

a1 :: Square
a1 = (1, 1)

h8 :: Square
h8 = (boardSize, boardSize)

isOnBoard :: Square -> Bool
isOnBoard (x, y) = all fileOrRankIsOnBoard [x, y]

fileOrRankIsOnBoard :: Int -> Bool
fileOrRankIsOnBoard x = (x >= 1) && (x <= boardSize)

isEmpty :: Board -> Square -> Bool
isEmpty board square = isNothing $ board ! square

allMoves :: Position -> [Move]
allMoves position = 
    [ move | 
     (piece, square) <- piecesToMoveInSquares position,
     move <- pieceMoves position piece square
    ]


piecesToMoveInSquares :: Position -> [(Piece, Square)]
piecesToMoveInSquares (Position board sideToMove) =
    [ (piece, square) | 
      (square, Just piece) <- assocs board, 
      getColor piece == sideToMove ]


pieceMoves :: Position -> Piece -> Square -> [Move]
pieceMoves position piece from = 
    [ Move from to | to <- tos ]
    where allLines = getLines from (getMovement piece)
          linesTos = [ cutLine position line | line <- allLines ]
          tos = concat linesTos


cutLine :: Position -> [Square] -> [Square]
cutLine (Position board sideToMove) line =
    exclude (coloredAs board sideToMove) candidates
    where candidates = takeWhileAndNext (isEmpty board) line


exclude :: (a -> Bool) -> [a] -> [a]
exclude p xs = [ x | x <- xs, not $ p x ]


coloredAs :: Board -> Color -> Square -> Bool
coloredAs board color square =
    case board ! square of
        Nothing -> False
        Just piece -> (getColor piece) == color

takeWhileAndNext :: (a -> Bool) -> [a] -> [a]
takeWhileAndNext p xs = 
    good ++ take 1 bad
    where (good, bad) = span p xs


getLines :: Square -> Movement -> [[Square]]
getLines square (Movement directions range) =
    [ filter isOnBoard [squareInDirection square d i | i <- [1..range]] | d <- directions ]


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
