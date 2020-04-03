{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import Uci.Input

main :: IO ()
main = hspec do
    describe "Uci.Input.parse" do
        it "parses `uci` command" do
            Uci.Input.parse "uci" `shouldBe` Uci.Input.Uci

        it "parses `isready` command" do
            Uci.Input.parse "isready" `shouldBe` Uci.Input.IsReady

        it "returns Unknown on unknown commands" do
            Uci.Input.parse "parse this" `shouldBe` Uci.Input.Unknown "parse this"