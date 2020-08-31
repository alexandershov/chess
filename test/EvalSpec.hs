{-# LANGUAGE BlockArguments #-}

module EvalSpec where

import Test.Hspec

import Engine
import Eval
import Fen
import Moves
import Pieces
import Position
import Squares


describeEval :: Spec
describeEval = do
    describePieceValues
    describeEvalPosition
    describeFindBestMove


describeFindBestMove :: Spec
describeFindBestMove = do
    describe "findBestMove" do
        it "maximizes piece movement" do
            findBestMove initialPosition `shouldBe` Move e2 e3 Nothing
        it "mates in one" do
            findBestMove mateInOnePosition `shouldBe` Move d8 h4 Nothing


mateInOnePosition :: Position
mateInOnePosition = 
    position
    where Right position = parsePosition "rnbqkbnr/pppp1ppp/4p3/8/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq g3 0 2"



describePieceValues :: Spec
describePieceValues = do
    describe "Piece value of" do
        it "queen is 9 pawns" do
            valueOf whiteQueen `shouldBe` 900
            valueOf blackQueen `shouldBe` -900
        it "rook is 5 pawns" do
            valueOf whiteRook `shouldBe` 500
            valueOf blackRook `shouldBe` -500
        it "bishop is 3.5 pawns" do
            valueOf whiteBishop `shouldBe` 350
            valueOf blackBishop `shouldBe` -350
        it "knight is 3 pawns" do
            valueOf whiteKnight `shouldBe` 300
            valueOf blackKnight `shouldBe` -300
        it "pawn is 1 pawn" do
            valueOf whitePawn `shouldBe` 100
            valueOf blackPawn `shouldBe` -100
        it "king is 0 pawns" do
            valueOf whiteKing `shouldBe` 0
            valueOf blackKing `shouldBe` 0


describeEvalPosition :: Spec
describeEvalPosition = do
    describe "Initial position" do
        it "is equal" do
            eval initialPosition `shouldBe` 0
    describe "Position after e2-e4" do
        it "is better for white" do
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" `evalShouldBe` 100
    describe "Position after mate" do
        it "is much better for the side who mates" do
            "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4" `evalShouldBe` 1000000
            "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3" `evalShouldBe` (-1000000)
    describe "Position after stalemate" do
        it "is equal" do
            "5bnr/4p1pq/4Qpkr/7p/7P/4P3/PPPP1PP1/RNB1KBNR b KQ - 2 10" `evalShouldBe` 0

evalShouldBe :: String -> Int -> IO ()
fen `evalShouldBe` score = 
    let Right position = parsePosition fen in
        eval position `shouldBe` score
