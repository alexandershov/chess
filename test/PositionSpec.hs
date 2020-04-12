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
        it "moves diagonally" do
            allMovesFrom e4 positionWithBishop `shouldMatchList` bishopE4Moves


describeRook :: Spec
describeRook = do
    describe "Rook" do
        it "moves by straight lines" do
            allMovesFrom b2 positionWithRook `shouldMatchList` rookB2Moves

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
    where board = put [whiteBishop `on` e4, blackPawn `on` c6, 
                       whiteKnight `on` g6]

positionWithRook :: Position
positionWithRook =
    Position board White
    where board = put [whiteRook `on` b2, whiteKing `on` e2, 
                       blackKnight `on` b5]


knightF3Moves :: [Move]
knightF3Moves = [Move f3 e5, Move f3 g5, Move f3 h4, Move f3 h2, Move f3 g1,
                 Move f3 d2, Move f3 d4]

bishopE4Moves :: [Move]
bishopE4Moves = [Move e4 d5, Move e4 c6, Move e4 f3, Move e4 g2, Move e4 h1,
                   Move e4 d3, Move e4 c2, Move e4 b1, Move e4 f5]

rookB2Moves :: [Move]
rookB2Moves = [Move b2 b3, Move b2 b4, Move b2 b5, Move b2 b1,
               Move b2 a2, Move b2 c2, Move b2 d2]


whiteKnight :: Piece
whiteKnight = Knight White

whiteBishop :: Piece
whiteBishop = Bishop White

whiteRook :: Piece
whiteRook = Rook White

whiteKing :: Piece
whiteKing = King White

blackPawn :: Piece
blackPawn = Pawn Black

blackKnight :: Piece
blackKnight = Knight Black

blackKing :: Piece
blackKing = King Black
