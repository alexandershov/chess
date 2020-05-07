{-# LANGUAGE BlockArguments #-}

module FenSpec where

import Data.Either (isLeft)

import Fen
import Pieces
import Squares

import Test.Hspec


describeFen :: Spec
describeFen = do
    describe "Fen" do
        it "parses side to move" do
            parseSideToMove "w" `shouldBe` Right White
            parseSideToMove "b" `shouldBe` Right Black
        it "handles errors when parsing side to move" do
            parseSideToMove "x" `shouldSatisfy` isLeft

        it "parses half move clock" do
            parseHalfMoveClock "10" `shouldBe` Right 10

        it "handles errors when parsing half move clock" do
            parseHalfMoveClock "x" `shouldSatisfy` isLeft

        it "parses en passant square" do
            parseEnPassant "e3" `shouldBe` (Right $ Just e3)

        it "parses missing en passant square" do
            parseEnPassant "-" `shouldBe` Right Nothing

        it "handles errors when parsing en passant" do
            parseEnPassant "x3" `shouldSatisfy` isLeft
