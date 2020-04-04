{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import qualified Uci


main :: IO ()
main = hspec do
    describeUciParse
    describeUciGetResponse
    describePlay

describeUciParse :: Spec
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

        it "parses `quit` command" do
            Uci.parse "quit" `shouldBe` Uci.Quit

        it "returns Unknown on unknown commands" do
            Uci.parse "parse this" `shouldBe` Uci.Unknown "parse this"

describeUciGetResponse :: Spec
describeUciGetResponse =
    describe "Uci.getResponse" do
        it "returns name and author on `uci` command" do
            Uci.Uci `responseShouldBe` ["id name chess", "id author Alexander Ershov", "uciok"]

        it "returns readyok on `isready` command" do
            Uci.IsReady `responseShouldBe` ["readyok"]

        it "returns nothing on `ucinewgame` command" do
            Uci.UciNewGame `responseShouldBe` []

        it "returns nothing on `position` command (for now)" do
            Uci.Position `responseShouldBe` []

        it "asks player on `go` command" do
            Uci.Go `responseShouldBe` ["bestmove e2e4"]

        it "returns error on unknown command" do
            Uci.Unknown "garbage" `responseShouldBe` ["unknown command garbage"]

        it "returns nothing on `quit` command" do
            Uci.Quit `responseShouldBe` []


describePlay :: Spec
describePlay = 
    describe "Uci.Play" do
        it "plays a game" do
            Uci.play quitter morphy nullWriter


responseShouldBe :: Uci.Command -> [String] -> IO ()
command `responseShouldBe` expectedLines = do
    response <- Uci.getResponse command morphy
    response `shouldBe` Uci.Response expectedLines


morphy :: IO String
morphy = return "e2e4"

quitter :: IO Uci.Command
quitter = return Uci.Quit

nullWriter :: Uci.Response -> IO ()
nullWriter _ = return ()