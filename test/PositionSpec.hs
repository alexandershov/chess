{-# LANGUAGE BlockArguments #-}

module PositionSpec where

import Data.Array ((//))

import Test.Hspec

import Position
import Squares

describeRook :: Spec
describeRook = do
    describe "Rook" do
            it "moves horizontally" do
                allMoves positionWithRook `shouldContain` [
                    Move a1 b1,
                    Move a1 c1,
                    Move a1 d1]
            it "moves vertically" do
                allMoves positionWithRook `shouldContain` [
                    Move a1 a2,
                    Move a1 a3,
                    Move a1 a4,
                    Move a1 a5]
            it "takes enemy piece" do
                allMoves positionWithRook `shouldContain` [Move a1 a5]
            it "doesn't take own piece" do
                allMoves positionWithRook `shouldNotContain` [Move a1 e1]
            it "can't move past enemy piece" do
                allMoves positionWithRook `shouldNotContain` [Move a1 a6]
            it "can't move past own piece" do
                allMoves positionWithRook `shouldNotContain` [Move a1 f1]


on :: Piece -> Square -> (Piece, Square)
piece `on` square = (piece, square)

put :: [(Piece, Square)] -> Board
put piecesOnSquares = 
    emptyBoard // [(square, Just piece) | (piece, square) <- piecesOnSquares]


positionWithRook :: Position
positionWithRook =
    let board = put [whiteRook `on` a1, 
                     whiteKing `on` e1, 
                     blackKnight `on` a5,
                     blackKing `on` e8] in
            Position board White

whiteRook :: Piece
whiteRook = Rook White

whiteKing :: Piece
whiteKing = King White

blackKnight :: Piece
blackKnight = Knight Black

blackKing :: Piece
blackKing = King Black
