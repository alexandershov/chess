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
            allMoves positionWithKnight `shouldContain` knightF3Moves
        it "takes enemy piece" do
            allMoves positionWithKnight `shouldContain` [Move f3 e5]
        it "doesn't takes own piece" do
            allMoves positionWithKnight `shouldNotContain` [Move f3 e1]
        it "doesn't jump too far" do
            allMoves positionWithKnight `shouldNotContain` [Move f3 c6]


describeBishop :: Spec
describeBishop = do
    describe "Bishop" do
            it "moves diagonally" do
                allMovesFrom e4 positionWithBishop `shouldMatchList` diagonalsFromE4


describeRook :: Spec
describeRook = do
    describe "Rook" do
            it "moves horizontally" do
                allMoves positionWithRook `shouldContain` lineA1D1
            it "moves vertically" do
                allMoves positionWithRook `shouldContain` lineA1A5
            it "takes enemy piece" do
                allMoves positionWithRook `shouldContain` [Move a1 a5]
            it "doesn't take own piece" do
                allMoves positionWithRook `shouldNotContain` [Move a1 e1]
            it "can't move past enemy piece" do
                allMoves positionWithRook `shouldNotContain` [Move a1 a6]
            it "can't move past own piece" do
                allMoves positionWithRook `shouldNotContain` [Move a1 f1]


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
    where board = put [whiteRook `on` a1, whiteKing `on` e1, 
                       blackKnight `on` a5, blackKing `on` e8]


knightF3Moves :: [Move]
knightF3Moves = [Move f3 g5, Move f3 h4, Move f3 h2, Move f3 g1,
                 Move f3 d2, Move f3 d4]

diagonalsFromE4 :: [Move]
diagonalsFromE4 = [Move e4 d5, Move e4 c6, Move e4 f3, Move e4 g2, Move e4 h1,
                   Move e4 d3, Move e4 c2, Move e4 b1, Move e4 f5]

lineA1D1 :: [Move]
lineA1D1 = [Move a1 b1, Move a1 c1, Move a1 d1]

lineA1A5 :: [Move]
lineA1A5 = [Move a1 a2, Move a1 a3, Move a1 a4, Move a1 a5]



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
