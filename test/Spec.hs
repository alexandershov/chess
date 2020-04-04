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
            Uci.Uci `responseShouldBe` Uci.Response ["id name chess", "id author Alexander Ershov"]

        it "returns readyok on `isready` command" do
            Uci.IsReady `responseShouldBe` Uci.Response ["readyok"]

        it "returns nothing on `ucinewgame` command" do
            Uci.UciNewGame `responseShouldBe` Uci.Response []

        it "returns nothing on `position` command (for now)" do
            Uci.Position `responseShouldBe` Uci.Response []

        it "asks player on `go` command" do
            Uci.Go `responseShouldBe` Uci.Response ["bestmove e2e4"]


responseShouldBe :: Uci.Command -> Uci.Response -> IO ()
command `responseShouldBe` expected = do
    response <- Uci.getResponse Morphy command
    response `shouldBe` expected


data TestPlayer = Morphy
instance Player TestPlayer where
    findBestMove Morphy = return "e2e4"
