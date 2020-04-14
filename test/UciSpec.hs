{-# LANGUAGE BlockArguments #-}

module UciSpec where

import Data.Either (isLeft)
import Data.IORef

import Test.Hspec

import qualified Position as P
import Position hiding (Position)
import Squares
import Uci

describeUciParse :: Spec
describeUciParse =
    describe "Uci.parse" do
        it "parses `uci` command" do
            parse "uci" `shouldBe` Uci

        it "parses `isready` command" do
            parse "isready" `shouldBe` IsReady

        it "parses `ucinewgame` command" do
            parse "ucinewgame" `shouldBe` UciNewGame

        it "parses valid `position` command" do
            parse "position startpos moves e2e4 e7e5" `shouldBe` Position positionAfterE4E5

        it "parses `position` command without moves" do
            parse "position startpos moves" `shouldBe` (Position $ Right initialPosition)

        it "handles invalid moves in `position` command" do
            let (Position parsed) = parse "position startpos moves e3e4 e7e5" in
                parsed `shouldSatisfy` isLeft

        it "handles invalid first part in `position` command" do
            let (Position parsed) = parse "position wrong moves e3e4 e7e5" in
                parsed `shouldSatisfy` isLeft

        it "handles missing `moves` in `position` command" do
            let (Position parsed) = parse "position startpos" in
                parsed `shouldSatisfy` isLeft

        it "handles bad files in `position` command" do
            let (Position parsed) = parse "position startpos moves k2e4" in
                parsed `shouldSatisfy` isLeft

        it "handles bad files in `position` command" do
            let (Position parsed) = parse "position startpos moves e9e4" in
                parsed `shouldSatisfy` isLeft

        it "parses `go` command" do
            parse "go" `shouldBe` Go

        it "parses `quit` command" do
            parse "quit" `shouldBe` Quit

        it "returns Unknown on unknown commands" do
            parse "parse this" `shouldBe` Unknown "parse this"

describeUciGetResponse :: Spec
describeUciGetResponse =
    describe "Uci.getResponse" do
        it "returns name and author on `uci` command" do
            Uci `responseShouldBe` ["id name chess", "id author Alexander Ershov", "uciok"]

        it "returns readyok on `isready` command" do
            IsReady `responseShouldBe` ["readyok"]

        it "returns nothing on `ucinewgame` command" do
            UciNewGame `responseShouldBe` []

        it "returns nothing on `position` command (for now)" do
            (Position $ Right initialPosition) `responseShouldBe` []

        it "asks player on `go` command" do
            Go `responseShouldBe` ["bestmove e2e4"]

        it "returns error on unknown command" do
            Unknown "garbage" `responseShouldBe` ["unknown command garbage"]

        it "returns nothing on `quit` command" do
            Quit `responseShouldBe` []


describePlay :: Spec
describePlay = 
    describe "Uci.Play" do
        it "plays a game" do
            responsesRef <- newIORef []
            commandsRef <- newIORef [Uci, IsReady, UciNewGame, 
                                     (Position $ Right initialPosition), 
                                     Go, Quit]

            play (commands commandsRef) morphy (refWriter responsesRef)

            responses <- readIORef responsesRef
            responses `shouldBe` 
                [uciResponse, 
                 readyOkResponse, 
                 emptyResponse, 
                 emptyResponse, 
                 bestMoveResponse "e2e4"]


responseShouldBe :: Command -> [String] -> IO ()
command `responseShouldBe` expectedLines = do
    response <- getResponse command morphy
    response `shouldBe` Response expectedLines


morphy :: IO String
morphy = return "e2e4"


commands :: IORef [Command] -> IO Command
commands ref = do
    curCommands <- readIORef ref
    modifyIORef ref tail
    return $ head curCommands

    
refWriter :: IORef [Response] -> Response -> IO ()
refWriter ref response = do
    modifyIORef ref (++ [response])


positionAfterE4E5 :: Either P.ErrorDesc P.Position
positionAfterE4E5 = do
    afterE4 <- initialPosition `make` Move e2 e4
    afterE5 <- afterE4 `make` Move e7 e5
    return afterE5
