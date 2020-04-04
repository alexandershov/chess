{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import Uci


main :: IO ()
main = hspec do
    describeUciParse
    describeUciGetResponse

describeUciParse :: SpecWith ()
describeUciParse =
    describe "Uci.parse" do
        it "parses `uci` command" do
            Uci.parse "uci" `shouldBe` Uci.Uci

        it "parses `isready` command" do
            Uci.parse "isready" `shouldBe` Uci.IsReady

        it "parses `ucinewgame` command" do
            Uci.parse "ucinewgame" `shouldBe` Uci.UciNewGame

        it "parses `position` command" do
            Uci.parse "position startpos moves e2e4" `shouldBe` Uci.Position

        it "parses `go` command" do
            Uci.parse "go" `shouldBe` Uci.Go

        it "returns Unknown on unknown commands" do
            Uci.parse "parse this" `shouldBe` Uci.Unknown "parse this"

describeUciGetResponse :: SpecWith ()
describeUciGetResponse =
    describe "Uci.getResponse" do
        it "returns name and author on `uci` command" do
            response <- Uci.getResponse Morphy Uci.Uci
            response `shouldBe` Uci.Response ["id name chess", "id author Alexander Ershov"]

        it "returns readyok on `isready` command" do
            response <- Uci.getResponse Morphy Uci.IsReady
            response `shouldBe` Uci.Response ["readyok"]

        it "returns nothing on `ucinewgame` command" do
            response <- Uci.getResponse Morphy Uci.UciNewGame
            response `shouldBe` Uci.Response []


data TestPlayer = Morphy
instance Player TestPlayer where
    findBestMove Morphy = do
        return "e2e4"
