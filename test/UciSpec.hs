{-# LANGUAGE BlockArguments #-}

module UciSpec where

import Data.IORef

import Test.Hspec

import Position
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

        it "parses `position` command" do
            parse "position startpos moves e2e4 e7e5" `shouldBe` Position

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
            Position `responseShouldBe` []

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
            commandsRef <- newIORef [Uci, IsReady, UciNewGame, Position, Go, Quit]

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

