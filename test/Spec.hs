{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import qualified Uci


main :: IO ()
main = hspec do
    describeUciParse
    describeUciGetResponse
    describePlay

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

        it "parses `quit` command" do
            Uci.parse "quit" `shouldBe` Uci.Quit

        it "returns Unknown on unknown commands" do
            Uci.parse "parse this" `shouldBe` Uci.Unknown "parse this"

describeUciGetResponse :: SpecWith ()
describeUciGetResponse =
    describe "Uci.getResponse" do
        it "returns name and author on `uci` command" do
            Uci.Uci `responseShouldBe` ["id name chess", "id author Alexander Ershov"]

        it "returns readyok on `isready` command" do
            Uci.IsReady `responseShouldBe` ["readyok"]

        it "returns nothing on `ucinewgame` command" do
            Uci.UciNewGame `responseShouldBe` []

        it "returns nothing on `position` command (for now)" do
            Uci.Position `responseShouldBe` []

        it "asks player on `go` command" do
            Uci.Go `responseShouldBe` ["bestmove e2e4"]


describePlay :: SpecWith ()
describePlay = 
    describe "Uci.Play" do
        it "plays a game" do
            Uci.play TestCommandReader morphy TestResponseWriter


responseShouldBe :: Uci.Command -> [String] -> IO ()
command `responseShouldBe` expectedLines = do
    response <- Uci.getResponse morphy command
    response `shouldBe` Uci.Response expectedLines


data TestCommandReader = TestCommandReader
data TestResponseWriter = TestResponseWriter

morphy :: IO String
morphy = return "e2e4"


instance Uci.CommandReader TestCommandReader where
    read _ = return Uci.Quit


instance Uci.ResponseWriter TestResponseWriter where
    write _ _ = return ()