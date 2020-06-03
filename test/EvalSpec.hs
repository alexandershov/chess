{-# LANGUAGE BlockArguments #-}

module EvalSpec where

import Test.Hspec

import Eval
import Pieces


describeEval :: Spec
describeEval = do
    describePieceValues


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