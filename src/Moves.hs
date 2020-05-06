{-# LANGUAGE NamedFieldPuns #-}

module Moves where

import Data.Array (assocs, (!), (//), listArray)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (partition)
import Data.Maybe (isJust, isNothing)

import Pieces
import Position hiding (board, sideToMove, castlingRights, enPassant, halfMoveClock)
import qualified Position as P
import Squares

boardSize :: Int
boardSize = 8

type Line = [Square]
showSquare :: Square -> String
showSquare (x, y) = 
    file ++ rank
    where file = [("abcdefgh" !! (x - 1))]
          rank = show y


data Move = Move Square Square Promotion deriving (Eq)

type Direction = (File, Rank)
type Range = Int
type ErrorDesc = String
type Promotion = Maybe Piece

data Movement = 
    PieceMovement [Direction] Range | 
    PawnMovement Direction Range [Direction]


instance Show Move where
    show (Move from to promotion) 
        | isOnBoard from && isOnBoard to = showSquare from ++ showSquare to ++ showPromotion promotion
        | otherwise = "MoveOutsideTheBoard " ++ show from ++ " -> " ++ show to ++ showPromotion promotion


showPromotion :: Promotion -> String
showPromotion Nothing = ""
showPromotion (Just (Pawn _)) = "p"
showPromotion (Just (Knight _)) = "N"
showPromotion (Just (Bishop _)) = "B"
showPromotion (Just (Rook _)) = "R"
showPromotion (Just (Queen _)) = "Q"
showPromotion (Just (King _)) = "K"


emptyBoard :: Board
emptyBoard = listArray (a1, h8) $ repeat Nothing


isOnBoard :: Square -> Bool
isOnBoard (x, y) = all fileOrRankIsOnBoard [x, y]


fileOrRankIsOnBoard :: Int -> Bool
fileOrRankIsOnBoard x = (x >= 1) && (x <= boardSize)


isEmpty :: Board -> Square -> Bool
isEmpty board square = isNothing $ board ! square


legalMoves :: Position -> [Move]
legalMoves position = 
    filter (`isLegalIn` position) moves
    where moves = allMoves position


isLegalIn :: Move -> Position -> Bool
move `isLegalIn` position@Position{P.sideToMove} = 
    not $ sideToMove `isUnderCheckIn` nextPosition
    where nextPosition = position `makeUnchecked` move


isUnderCheckIn :: Color -> Position -> Bool
color `isUnderCheckIn` position =
    any (threatens position king) moves
    where moves = allSimpleMoves position
          king = King color


threatens :: Position -> Piece -> Move -> Bool
threatens Position{P.board} piece (Move _ to _) =
    board ! to == Just piece


allSimpleMoves :: Position -> [Move]
allSimpleMoves position = 
    [ move | 
     (piece, square) <- piecesToMoveInSquares position,
     move <- pieceMoves position piece square
    ]

allMoves :: Position -> [Move]
allMoves position = allSimpleMoves position ++ allCastleMoves position


allCastleMoves :: Position -> [Move]
allCastleMoves position
    | canLong && canShort = [long, short]
    | canLong = [long]
    | canShort = [short]
    | otherwise = []
    where canLong = canCastleLong position
          canShort = canCastleShort position
          long = createLongCastleMove position
          short = createShortCastleMove position


canCastleLong :: Position -> Bool
canCastleLong position@Position{P.sideToMove, P.castlingRights} = 
    possible && noObstacles && noThreats
    where possible = S.member LongCastle (castlingRights M.! sideToMove)
          noObstacles = hasEmptySquaresForLongCastle position
          noThreats = hasSafeLongCastle position


canCastleShort :: Position -> Bool
canCastleShort position@Position{P.sideToMove, P.castlingRights} = 
    possible && noObstacles && noThreats
    where possible = S.member ShortCastle (castlingRights M.! sideToMove)
          noObstacles = hasEmptySquaresForShortCastle position
          noThreats = hasSafeShortCastle position


hasEmptySquaresForShortCastle :: Position -> Bool
hasEmptySquaresForShortCastle Position{P.board, P.sideToMove=White} =
    and $ map (isEmpty board) [f1, g1]
hasEmptySquaresForShortCastle Position{P.board, P.sideToMove=Black} =
    and $ map (isEmpty board) [f8, g8]


hasEmptySquaresForLongCastle :: Position -> Bool
hasEmptySquaresForLongCastle Position{P.board, P.sideToMove=White} =
    and $ map (isEmpty board) [d1, c1, b1]
hasEmptySquaresForLongCastle Position{P.board, P.sideToMove=Black} =
    and $ map (isEmpty board) [d8, c8, b8]


hasSafeLongCastle :: Position -> Bool
hasSafeLongCastle position@Position{P.sideToMove=White} = 
    not $ position `hasThreatTo` [c1, d1, e1]

hasSafeLongCastle position@Position{P.sideToMove=Black} = 
    not $ position `hasThreatTo` [c8, d8, e8]

hasSafeShortCastle :: Position -> Bool
hasSafeShortCastle position@Position{P.sideToMove=White} = 
    not $ position `hasThreatTo` [e1, f1, g1]

hasSafeShortCastle position@Position{P.sideToMove=Black} = 
    not $ position `hasThreatTo` [e8, f8, g8]


hasThreatTo :: Position -> [Square] -> Bool
hasThreatTo Position{P.board, P.sideToMove, P.castlingRights} squares =
    or [to `elem` squares | (Move _ to _) <- threats]
    where threats = allSimpleMoves (Position board (rival sideToMove) castlingRights Nothing 0 M.empty)


createShortCastleMove :: Position -> Move
createShortCastleMove Position{P.sideToMove=White} = Move e1 g1 Nothing
createShortCastleMove Position{P.sideToMove=Black} = Move e8 g8 Nothing


createLongCastleMove :: Position -> Move
createLongCastleMove Position{P.sideToMove=White} = Move e1 c1 Nothing
createLongCastleMove Position{P.sideToMove=Black} = Move e8 c8 Nothing


piecesToMoveInSquares :: Position -> [(Piece, Square)]
piecesToMoveInSquares Position{P.board, P.sideToMove} =
    [ (piece, square) | 
      (square, Just piece) <- assocs board, 
      getColor piece == sideToMove ]


pieceMoves :: Position -> Piece -> Square -> [Move]
pieceMoves position piece from = 
    getMoves position movement from
    where movement = getMovement piece from


getMoves :: Position -> Movement -> Square -> [Move]
getMoves position (PieceMovement directions range) from = 
    [ Move from to Nothing | to <- tos ]
    where slightlyLongLines = getLines from directions range
          legalLines = [ cutLine position line | line <- slightlyLongLines ]
          tos = concat legalLines


getMoves position@Position{P.sideToMove} (PawnMovement moveDirection range captureDirections) from =
    simpleMoves ++ promotions
    where movesTos = getPawnMovesTos position moveDirection range from
          capturesTos = getPawnCapturesTos position captureDirections from
          tos = movesTos ++ capturesTos
          (promotionTos, simpleTos) = partition (isInPromotionRank sideToMove) tos
          simpleMoves = [ Move from to Nothing | to <- simpleTos ]
          pieces = [(Knight sideToMove), (Bishop sideToMove), (Rook sideToMove), (Queen sideToMove)]
          promotions = [ Move from to (Just piece) | to <- promotionTos, piece <- pieces ]


isInPromotionRank :: Color -> Square -> Bool
isInPromotionRank White (_, rank) = rank == boardSize
isInPromotionRank Black (_, rank) = rank == 1


getPawnMovesTos :: Position -> Direction -> Range -> Square -> [Square]
getPawnMovesTos Position{P.board} direction range from = 
    concat legalLines
    where slightlyLongLines = getLines from [direction] range
          legalLines = [ takeWhile (isEmpty board) line | line <- slightlyLongLines ]


getPawnCapturesTos :: Position -> [Direction] -> Square -> [Square]
getPawnCapturesTos position directions from =
    concat legalLines
    where slightyLongLines = getLines from directions 1
          legalLines = [ takeWhile (canBeCapturedByPawn position) line | line <- slightyLongLines ]


cutLine :: Position -> Line -> Line
cutLine position@Position{P.board} line =
    exclude (isOccupiedBySideToMove position) squares
    where squares = takeWhileWithBreaker (isEmpty board) line


exclude :: (a -> Bool) -> [a] -> [a]
exclude p xs = [ x | x <- xs, not $ p x ]


isOccupiedBySideToMove :: Position -> Square -> Bool
isOccupiedBySideToMove position@Position{P.sideToMove} square =
    isOccupiedByColor position sideToMove square


isOccupiedByRival :: Position -> Square -> Bool
isOccupiedByRival position@Position{P.sideToMove} square =
    isOccupiedByColor position (rival sideToMove) square


canBeCapturedByPawn :: Position -> Square -> Bool
canBeCapturedByPawn position@Position{P.enPassant} square =
    (isOccupiedByRival position square) || (Just square == enPassant)


isOccupiedByColor :: Position -> Color -> Square -> Bool
isOccupiedByColor Position{P.board} color square =
    case board ! square of
        Nothing -> False
        Just piece -> (getColor piece) == color


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
getMovement (Pawn color) from = 
    PawnMovement moveDirection range captureDirections
    where moveDirection = pawnMoveDirection color
          range = pawnRange color from
          captureDirections = pawnCaptureDirections color

getMovement (Knight _) _ = PieceMovement jumps 1
getMovement (Bishop _) _ = PieceMovement diagonals boardSize
getMovement (Rook _) _ = PieceMovement straightLines boardSize
getMovement (Queen _) _ = PieceMovement (diagonals ++ straightLines) boardSize
getMovement (King _) _ = PieceMovement (diagonals ++ straightLines) 1


pawnMoveDirection :: Color -> Direction
pawnMoveDirection White = (0, 1)
pawnMoveDirection Black = (0, -1)

pawnCaptureDirections :: Color -> [Direction]
pawnCaptureDirections White = [(1, 1), (-1, 1)]
pawnCaptureDirections Black = [(1, -1), (-1, -1)]

startingPawnRank :: Color -> Rank
startingPawnRank White = 2
startingPawnRank Black = 7

pawnRange :: Color -> Square -> Int
pawnRange color (_, rank) =
    if (rank == startingPawnRank color) then 2 else 1


jumps :: [Direction]
jumps = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

diagonals :: [Direction]
diagonals = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

straightLines :: [Direction]
straightLines = [(1, 0), (0, 1), (-1, 0), (0, -1)]

on :: Piece -> Square -> (Piece, Square)
piece `on` square = (piece, square)


putOnBoard :: Board -> [(Piece, Square)] -> Board
putOnBoard board piecesOnSquares = 
    board // [(square, Just piece) | (piece, square) <- piecesOnSquares]


put :: [(Piece, Square)] -> Board
put = putOnBoard emptyBoard


initialPosition :: Position
initialPosition = 
    position'{P.repetitions=repetitions}
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
        position' = Position board White fullCastlingRights Nothing 0 M.empty
        repetitions = getNextRepetitions position'


fullCastlingRights :: CastlingRights
fullCastlingRights = 
    M.fromList [(White, bothCastles), (Black, bothCastles)]


bothCastles :: S.Set Castle
bothCastles = S.fromList [LongCastle, ShortCastle]


makeUnchecked :: Position -> Move -> Position
position@Position{P.board, P.sideToMove, P.castlingRights, P.repetitions} `makeUnchecked` move@(Move from to _) =
    nextPosition'{P.repetitions=nextRepetitions}
    where nextBoard = getNextBoard position move
          tmpCastlingRights = getNextCastlingRights board sideToMove castlingRights from
          nextCastlingRights = getNextCastlingRights board (rival sideToMove) tmpCastlingRights to
          nextEnPassant = getNextEnPassant position move
          nextHalfMoveClock = getNextHalfMoveClock position move
          nextPosition' = Position nextBoard (rival sideToMove) nextCastlingRights nextEnPassant nextHalfMoveClock repetitions
          nextRepetitions = getNextRepetitions nextPosition'


make :: Position -> Move -> Either ErrorDesc Position
position `make` move =
    case move `elem` (legalMoves position) of
        True -> do
            return $ position `makeUnchecked` move
        False -> Left $ "move " ++ (show move) ++ " is illegal"


getNextCastlingRights :: Board -> Color -> CastlingRights -> Square -> CastlingRights
getNextCastlingRights board color castlingRights square
    | kingTouched board square = without castlingRights color [LongCastle, ShortCastle]
    | queenRookTouched color square = without castlingRights color [LongCastle]
    | kingRookTouched color square = without castlingRights color [ShortCastle]
    | otherwise = castlingRights


getNextEnPassant :: Position -> Move -> Maybe Square
getNextEnPassant position move@(Move (fromFile, fromRank) (_, toRank) _) =
    if isPawn && doubleMove then Just (fromFile, rank) else Nothing
    where isPawn = isPawnMove position move
          doubleMove = abs (toRank - fromRank) == 2
          rank = (fromRank + toRank) `div` 2


isPawnMove :: Position -> Move -> Bool
isPawnMove Position{P.board, P.sideToMove} (Move from _ _) =
    board ! from == Just (Pawn sideToMove)


isCapture :: Position -> Move -> Bool
isCapture position move@(Move _ to _) =
    isEnPassant position move || isOccupiedByRival position to


getNextHalfMoveClock :: Position -> Move -> Int
getNextHalfMoveClock position@Position{P.halfMoveClock} move = 
    if isPawn || isCapture position move then 0 else halfMoveClock + 1
    where isPawn = isPawnMove position move


getNextRepetitions :: Position -> M.Map Position Int
getNextRepetitions nextPosition'@Position{P.repetitions} =
    case M.lookup essence repetitions of
        Nothing -> M.insert essence 1 repetitions
        Just n -> M.insert essence (n + 1) repetitions
    where essence = nextPosition'{P.halfMoveClock=0, P.repetitions=M.empty}


getNextBoard :: Position -> Move -> Board
getNextBoard position move
    | isShortCastle position move = castleShort position
    | isLongCastle position move = castleLong position
    | isEnPassant position move = takeEnPassant position move
    | otherwise = moveChessman position move


isShortCastle :: Position -> Move -> Bool
isShortCastle Position{P.board, P.sideToMove=White} (Move from to _) =
    movesKing && from == e1 && to == g1
    where movesKing = board ! from == Just (King White)

isShortCastle Position{P.board, P.sideToMove=Black} (Move from to _) =
    movesKing && from == e8 && to == g8
    where movesKing = board ! from == Just (King Black)


isLongCastle :: Position -> Move -> Bool
isLongCastle Position{P.board, P.sideToMove=White} (Move from to _) =
    movesKing && from == e1 && to == c1
    where movesKing = board ! from == Just (King White)

isLongCastle Position{P.board, P.sideToMove=Black} (Move from to _) =
    movesKing && from == e8 && to == c8
    where movesKing = board ! from == Just (King Black)


castleShort :: Position -> Board
castleShort Position{P.board, P.sideToMove=White} =
    board // [(e1, Nothing), (g1, Just whiteKing), (h1, Nothing), (f1, Just whiteRook)]
castleShort Position{P.board, P.sideToMove=Black} =
    board // [(e8, Nothing), (g8, Just blackKing), (h8, Nothing), (f8, Just blackRook)]


castleLong :: Position -> Board
castleLong Position{P.board, P.sideToMove=White} =
    board // [(e1, Nothing), (c1, Just whiteKing), (a1, Nothing), (d1, Just whiteRook)]
castleLong Position{P.board, P.sideToMove=Black} =
    board // [(e8, Nothing), (c8, Just blackKing), (a8, Nothing), (d8, Just blackRook)]


isEnPassant :: Position -> Move -> Bool
isEnPassant position@Position{P.enPassant} move@(Move _ to _) =
    isPawnMove position move && Just to == enPassant


takeEnPassant :: Position -> Move -> Board
takeEnPassant position@Position{P.sideToMove, P.enPassant} move =
    case enPassant of
        Nothing -> error $ "impossible " ++ (show position) ++ " should have en passant"
        Just square -> board // [(getEnPassantReality square (rival sideToMove), Nothing)]
    where board = moveChessman position move


getEnPassantReality :: Square -> Color -> Square
getEnPassantReality (file, rank) White = (file, rank + 1)
getEnPassantReality (file, rank) Black = (file, rank - 1)


moveChessman :: Position -> Move -> Board
moveChessman Position{P.board} (Move from to promotion) =
    board // [(from, Nothing), (to, dstPiece)]
    where srcPiece = board ! from
          dstPiece = if isJust promotion then promotion else srcPiece


queenRookTouched  :: Color -> Square -> Bool
queenRookTouched color (file, rank) =
    file == 1 && rank == rankWithPieces color


kingRookTouched :: Color -> Square -> Bool
kingRookTouched color (file, rank) =
    file == boardSize && rank == rankWithPieces color


kingTouched :: Board -> Square -> Bool
kingTouched board square =
    case board ! square of
        Just (King _) -> True
        _ -> False

rankWithPieces :: Color -> Rank
rankWithPieces White = 1
rankWithPieces Black = boardSize


without :: CastlingRights -> Color -> [Castle] -> CastlingRights
without castlingRights color castles =
    M.insert color newCastles castlingRights
    where newCastles = S.difference bothCastles $ S.fromList castles


rival :: Color -> Color
rival White = Black
rival Black = White