module Position where

import Data.Array (assocs, Array, (!), listArray)
import Data.Maybe (isNothing)

boardSize :: Int
boardSize = 8

data Color = Black | White deriving (Eq, Show)
data Piece = 
    Pawn { getColor :: Color } | 
    Knight { getColor :: Color } | 
    Bishop { getColor :: Color } |
    Rook { getColor :: Color } | 
    Queen { getColor :: Color } | 
    King { getColor :: Color }

type Square = (Int, Int)
type Line = [Square]
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
    where slightlyLongLines = getLines from (getMovement piece)
          legalLines = [ cutLine position line | line <- slightlyLongLines ]
          tos = concat legalLines


cutLine :: Position -> Line -> Line
cutLine position@(Position board _) line =
    exclude (isOccupiedBySideToMove position) squares
    where squares = takeWhileWithBreaker (isEmpty board) line


exclude :: (a -> Bool) -> [a] -> [a]
exclude p xs = [ x | x <- xs, not $ p x ]


isOccupiedBySideToMove :: Position -> Square -> Bool
isOccupiedBySideToMove (Position board color) square =
    case board ! square of
        Nothing -> False
        Just piece -> (getColor piece) == color


takeWhileWithBreaker :: (a -> Bool) -> [a] -> [a]
takeWhileWithBreaker p xs = 
    good ++ take 1 bad
    where (good, bad) = span p xs


getLines :: Square -> Movement -> [Line]
getLines from (Movement directions range) =
    [ lineInDirection from d range | d <- directions ]


lineInDirection :: Square -> Direction -> Range -> Line
lineInDirection from direction range =
    filter isOnBoard [squareInDirection from direction i | i <- [1..range]]


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

jumps :: [Direction]
jumps = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

diagonals :: [Direction]
diagonals = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

straightLines :: [Direction]
straightLines = [(1, 0), (0, 1), (-1, 0), (0, -1)]
