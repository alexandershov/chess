{-# LANGUAGE BlockArguments #-}

module FenSpec where

import Data.Either (isLeft, isRight)
import qualified Data.Map as M

import Fen
import Moves
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

        it "parses white pieces" do
            parseRankElement 'R' `shouldBe` Right [Just whiteRook]
            parseRankElement 'N' `shouldBe` Right [Just whiteKnight]
            parseRankElement 'B' `shouldBe` Right [Just whiteBishop]
            parseRankElement 'Q' `shouldBe` Right [Just whiteQueen]
            parseRankElement 'K' `shouldBe` Right [Just whiteKing]
            parseRankElement 'P' `shouldBe` Right [Just whitePawn]

        it "parses black pieces" do
            parseRankElement 'r' `shouldBe` Right [Just blackRook]
            parseRankElement 'n' `shouldBe` Right [Just blackKnight]
            parseRankElement 'b' `shouldBe` Right [Just blackBishop]
            parseRankElement 'q' `shouldBe` Right [Just blackQueen]
            parseRankElement 'k' `shouldBe` Right [Just blackKing]
            parseRankElement 'p' `shouldBe` Right [Just blackPawn]

        it "parses empty squares" do
            parseRankElement '1' `shouldBe` Right [Nothing]
            parseRankElement '3' `shouldBe` Right [Nothing, Nothing, Nothing]
            parseRankElement '8' `shouldSatisfy` isRight

        it "handles errors when parsing rank element" do
            parseRankElement 'x' `shouldSatisfy` isLeft
            parseRankElement '0' `shouldSatisfy` isLeft
            parseRankElement '9' `shouldSatisfy` isLeft

        it "parses castling rights" do
            parseCastlingRights "KkQq" `shouldBe` Right fullCastlingRights

        it "parses missing castling rights" do
            parseCastlingRights "-" `shouldBe` Right M.empty

        it "handles errors when parsing castling rights" do
            parseCastlingRights "X" `shouldSatisfy` isLeft
            parseCastlingRights "kk" `shouldSatisfy` isLeft

        it "parses an initial position" do
            parsePosition initialFen `shouldBe` Right initialPosition
            where initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 