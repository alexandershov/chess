{-# LANGUAGE BlockArguments #-}

module FenSpec where

import Data.Either (isLeft)
import Fen
import Pieces

import Test.Hspec


describeFen :: Spec
describeFen = do
    describe "Fen" do
        it "parses side to move" do
            parseSideToMove "w" `shouldBe` Right White
            parseSideToMove "b" `shouldBe` Right Black
        it "handles errors when parses side to move" do
            parseSideToMove "x" `shouldSatisfy` isLeft
