{-# LANGUAGE BlockArguments #-}

module PositionSpec where

import Data.Array ((//))

import Test.Hspec

import Position hiding (a1, h8)
import Squares


describeKnight :: Spec
describeKnight = do
    describe "Knight" do
        it "jumps" do
            allMovesFrom f3 positionWithKnight `shouldMatchList` knightF3Moves


describeBishop :: Spec
describeBishop = do
    describe "Bishop" do
        it "moves by diagonals" do
            allMovesFrom b2 positionWithBishop `shouldMatchList` bishopB2Moves


describeRook :: Spec
describeRook = do
    describe "Rook" do
        it "moves by straight lines" do
            allMovesFrom b2 positionWithRook `shouldMatchList` rookB2Moves

describeQueen :: Spec
describeQueen = do
    describe "Queen" do
        it "moves by diagonals and straight lines" do
            allMovesFrom b2 positionWithQueen `shouldMatchList` queenB2Moves


describeKing :: Spec
describeKing = do
    describe "King" do
        it "moves one square in each direction" do
            allMovesFrom e1 positionWithKing `shouldMatchList` kingE1Moves

allMovesFrom :: Square -> Position -> [Move]
allMovesFrom square position =
    [ move | move@(Move from _) <- allMoves position, from == square ]


on :: Piece -> Square -> (Piece, Square)
piece `on` square = (piece, square)

put :: [(Piece, Square)] -> Board
put piecesOnSquares = 
    emptyBoard // [(square, Just piece) | (piece, square) <- piecesOnSquares]

positionWithKnight :: Position
positionWithKnight = 
    Position board White
    where board = put [whiteKnight `on` f3, blackPawn `on` e5, 
                       whiteRook `on` e1, whiteBishop `on` f2]


positionWithBishop :: Position
positionWithBishop = 
    Position board White
    where board = put [whiteBishop `on` b2, blackPawn `on` e5, 
                       whiteKnight `on` c1]

positionWithRook :: Position
positionWithRook =
    Position board White
    where board = put [whiteRook `on` b2, whiteKing `on` e2, 
                       blackKnight `on` b5]


positionWithQueen :: Position
positionWithQueen =
    Position board White
    where board = put [whiteQueen `on` b2, whiteKing `on` e2, 
                       blackKnight `on` b5, blackPawn `on` e5,
                       whiteKnight `on` c1]


positionWithKing :: Position
positionWithKing =
    Position board White
    where board = put [whiteKing `on` e1, blackPawn `on` e2, 
                       whiteKnight `on` d2]


knightF3Moves :: [Move]
knightF3Moves = [Move f3 e5, Move f3 g5, Move f3 h4, Move f3 h2, Move f3 g1,
                 Move f3 d2, Move f3 d4]

bishopB2Moves :: [Move]
bishopB2Moves = [Move b2 a1, Move b2 a3, Move b2 c3, Move b2 d4, Move b2 e5]

rookB2Moves :: [Move]
rookB2Moves = [Move b2 b3, Move b2 b4, Move b2 b5, Move b2 b1,
               Move b2 a2, Move b2 c2, Move b2 d2]


queenB2Moves :: [Move]
queenB2Moves = bishopB2Moves ++ rookB2Moves

kingE1Moves :: [Move]
kingE1Moves = [Move e1 f1, Move e1 d1, Move e1 e2, Move e1 f2]


whiteKnight :: Piece
whiteKnight = Knight White

whiteBishop :: Piece
whiteBishop = Bishop White

whiteRook :: Piece
whiteRook = Rook White

whiteQueen :: Piece
whiteQueen = Queen White

whiteKing :: Piece
whiteKing = King White

blackPawn :: Piece
blackPawn = Pawn Black

blackKnight :: Piece
blackKnight = Knight Black

blackKing :: Piece
blackKing = King Black
