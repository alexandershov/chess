{-# LANGUAGE BlockArguments #-}

module PositionSpec where

import Data.Array ((//))

import Test.Hspec

import Position
import Squares

describeRookMoves :: Spec
describeRookMoves = do
    describe "rookMoves" do
            it "moves horizontally" do
                allMoves rookTestPosition `shouldContain` [Move a1 d1]
            it "moves vertically" do
                allMoves rookTestPosition `shouldContain` [Move a1 a4]
            it "takes enemy piece" do
                allMoves rookTestPosition `shouldContain` [Move a1 a5]
            it "doesn't take own piece" do
                allMoves rookTestPosition `shouldNotContain` [Move a1 e1]
            it "can't move past enemy piece" do
                allMoves rookTestPosition `shouldNotContain` [Move a1 a6]
            it "can't move past own piece" do
                allMoves rookTestPosition `shouldNotContain` [Move a1 f1]


on :: Piece -> Square -> (Piece, Square)
piece `on` square = (piece, square)

put :: [(Piece, Square)] -> Board
put piecesOnSquares = 
    emptyBoard // [(square, Just piece) | (piece, square) <- piecesOnSquares]


rookTestPosition :: Position
rookTestPosition =
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
