module Position where

import Data.Array (assocs, Array, (!), (//), listArray)
import Data.Maybe (isNothing)

import Pieces
import Squares

boardSize :: Int
boardSize = 8

type Line = [Square]
showSquare :: Square -> String
showSquare (x, y) = 
    file ++ rank
    where file = [("abcdefgh" !! (x - 1))]
          rank = show y

type Board = Array Square (Maybe Piece)

data Position = Position Board Color deriving (Eq, Show)
data Move = Move Square Square deriving (Eq)

type Direction = (Int, Int)
type Range = Int
type ErrorDesc = String

data Movement = PieceMovement [Direction] Range | PawnMovement [Direction] Range

instance Show Move where
    show (Move from to) 
        | isOnBoard from && isOnBoard to = showSquare from ++ showSquare to
        | otherwise = "MoveOutsideTheBoard " ++ (show from) ++ " -> " ++ (show to)

emptyBoard :: Board
emptyBoard = listArray (a1, h8) $ repeat Nothing

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
    where movement = getMovement piece from
          tos = getTos position movement from


getTos :: Position -> Movement -> Square -> [Square]
getTos position (PieceMovement directions range) from = 
    concat legalLines
    where slightlyLongLines = getLines from directions range
          legalLines = [ cutLine position line | line <- slightlyLongLines ]


getTos (Position board _) (PawnMovement direction range) from =
    concat legalLines
    where slightlyLongLines = getLines from direction range
          legalLines = [ takeWhile (isEmpty board) line | line <- slightlyLongLines ]


cutLine :: Position -> Line -> Line
cutLine position@(Position board _) line =
    exclude (isOccupiedBySideToMove position) squares
    where squares = takeWhileWithBreaker (isEmpty board) line


exclude :: (a -> Bool) -> [a] -> [a]
exclude p xs = [ x | x <- xs, not $ p x ]


isOccupiedBySideToMove :: Position -> Square -> Bool
isOccupiedBySideToMove (Position board sideToMove) square =
    case board ! square of
        Nothing -> False
        Just piece -> (getColor piece) == sideToMove


takeWhileWithBreaker :: (a -> Bool) -> [a] -> [a]
takeWhileWithBreaker p xs = 
    good ++ take 1 bad
    where (good, bad) = span p xs


getLines :: Square -> [Direction] -> Range -> [Line]
getLines from directions range =
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


getMovement :: Piece -> Square -> Movement
getMovement (Pawn White) (_, rank) = 
    case rank of
        2 ->  PawnMovement whitePawnMoveDirection 2
        _ -> PawnMovement whitePawnMoveDirection 1


getMovement (Pawn Black) (_, rank) = 
    case rank of
        7 ->  PawnMovement blackPawnMoveDirection 2
        _ -> PawnMovement blackPawnMoveDirection 1

getMovement (Knight _) _ = PieceMovement jumps 1
getMovement (Bishop _) _ = PieceMovement diagonals boardSize
getMovement (Rook _) _ = PieceMovement straightLines boardSize
getMovement (Queen _) _ = PieceMovement (diagonals ++ straightLines) boardSize
getMovement (King _) _ = PieceMovement (diagonals ++ straightLines) 1


whitePawnMoveDirection :: [Direction]
whitePawnMoveDirection = [(0, 1)]


blackPawnMoveDirection :: [Direction]
blackPawnMoveDirection = [(0, -1)]


jumps :: [Direction]
jumps = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

diagonals :: [Direction]
diagonals = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

straightLines :: [Direction]
straightLines = [(1, 0), (0, 1), (-1, 0), (0, -1)]

on :: Piece -> Square -> (Piece, Square)
piece `on` square = (piece, square)

put :: [(Piece, Square)] -> Board
put piecesOnSquares = 
    emptyBoard // [(square, Just piece) | (piece, square) <- piecesOnSquares]


initialPosition :: Position
initialPosition = 
    Position board White
    where 
        board = put $ firstRank ++ secondRank ++ seventhRank ++ eightRank
        firstRank = [whiteRook `on` a1, whiteKnight `on` b1, whiteBishop `on` c1,
                     whiteQueen `on` d1, whiteKing `on` e1, 
                     whiteBishop `on` f1, whiteKnight `on` g1, whiteRook `on` h1]

        secondRank = map (whitePawn `on`) [a2, b2, c2, d2, e2, f2, g2, h2]
        seventhRank = map (blackPawn `on`) [a7, b7, c7, d7, e7, f7, g7, h7]

        eightRank = [blackRook `on` a8, blackKnight `on` b8, blackBishop `on` c8,
                     blackQueen `on` d8, blackKing `on` e8, 
                     blackBishop `on` f8, blackKnight `on` g8, blackRook `on` h8]


make :: Position -> Move -> Either ErrorDesc Position
(Position board sideToMove) `make` (Move from to) = 
    case maybePiece of
        Nothing -> Left $ showSquare from ++ " square is empty"
        _ -> Right $ Position boardAfterMove (rival sideToMove)
    where maybePiece = board ! from
          boardAfterMove = board // [(from, Nothing), (to, maybePiece)]


rival :: Color -> Color
rival White = Black
rival Black = White