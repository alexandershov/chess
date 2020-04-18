{-# LANGUAGE BlockArguments #-}

module PositionSpec where

import Data.Array ((!), elems)
import Data.Either (isLeft)

import Test.Hspec

import Pieces
import Position
import Squares

describePieces :: Spec
describePieces = do
    describeWhitePawn
    describeBlackPawn
    describeKnight
    describeBishop
    describeRook
    describeQueen
    describeKing

describeWhitePawn :: Spec
describeWhitePawn = do
    describe "white pawn" do
        it "moves forward by 1 square" do
            allMovesFrom e3 positionWithWhitePawn `shouldMatchList` [Move e3 e4]
        it "moves forward by 2 squares from the initial position" do
            allMovesFrom d2 positionWithWhitePawn `shouldMatchList` [Move d2 d3, Move d2 d4]
        it "stops at own piece" do
            allMovesFrom c4 positionWithWhitePawn `shouldMatchList` []
        it "stops at enemy piece" do
            allMovesFrom f4 positionWithWhitePawn `shouldMatchList` []
        it "captures enemy pieces" do
            allMovesFrom g2 positionWithWhitePawn `shouldMatchList` [Move g2 h3, Move g2 f3]
        it "doesn't capture own pieces" do
            allMovesFrom d2 positionWithWhitePawn `shouldMatchList` [Move d2 d3, Move d2 d4]


describeBlackPawn :: Spec
describeBlackPawn = do
    describe "black pawn" do
        it "moves forward by 1 square" do
            allMovesFrom e6 positionWithBlackPawn `shouldMatchList` [Move e6 e5]
        it "moves forward by 2 squares from the initial position" do
            allMovesFrom d7 positionWithBlackPawn `shouldMatchList` [Move d7 d6, Move d7 d5]
        it "stops at own piece" do
            allMovesFrom c5 positionWithBlackPawn `shouldMatchList` []
        it "stops at enemy piece" do
            allMovesFrom f5 positionWithBlackPawn `shouldMatchList` []
        it "captures enemy pieces" do
            allMovesFrom g7 positionWithBlackPawn `shouldMatchList` [Move g7 h6, Move g7 f6]
        it "doesn't capture own pieces" do
            allMovesFrom d7 positionWithBlackPawn `shouldMatchList` [Move d7 d6, Move d7 d5]

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


describeInitialPosition :: Spec
describeInitialPosition = do
    describe "initialPosition" do
            it "has 32 pieces" do
                sum [ 1 | Just _ <- elems board ] `shouldBe` (32::Int)
            it "has white as side to move" do
                sideToMove `shouldBe` White
            where Position board sideToMove = initialPosition


describeMakeMove :: Spec
describeMakeMove = do
    describe "making a move" do
        it "moves a piece" do
            (board ! g1) `shouldBe` Nothing
            (board ! f3) `shouldBe` Just whiteKnight
        it "changes a side to move" do
            sideToMove `shouldBe` Black
        it "returns Left if there's no piece in the square" do
            initialPosition `make` Move f3 g1 `shouldSatisfy` isLeft
    where Right (Position board sideToMove) = initialPosition `make` Move g1 f3
      

allMovesFrom :: Square -> Position -> [Move]
allMovesFrom square position =
    [ move | move@(Move from _) <- allMoves position, from == square ]


positionWithWhitePawn :: Position
positionWithWhitePawn = 
    Position board White
    where board = put [whitePawn `on` e3, whitePawn `on` d2,
                       whitePawn `on` c4, whitePawn `on` c5,
                       whitePawn `on` f4, blackPawn `on` f5,
                       whitePawn `on` g2, blackPawn `on` g3,
                       blackKnight `on` f3, blackBishop `on` h3]


positionWithBlackPawn :: Position
positionWithBlackPawn = 
    Position board Black
    where board = put [blackPawn `on` e6, blackPawn `on` d7,
                       blackPawn `on` c5, blackPawn `on` c4,
                       blackPawn `on` f5, whitePawn `on` f4,
                       blackPawn `on` g7, whitePawn `on` g6,
                       whiteKnight `on` f6, whiteBishop `on` h6]


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
