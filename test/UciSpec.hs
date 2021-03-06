{-# LANGUAGE BlockArguments #-}

module UciSpec where

import Data.Either (isLeft, isRight)
import Data.IORef

import Test.Hspec

import Moves
import qualified Position as P
import Squares
import Uci


describeUci :: Spec
describeUci = do
    describeUciParse
    describeUciGetResponse
    describeUciPlay


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

        it "parses moves with promotions" do
            let movesWithPromotion = "f2f4 a7a6 f4f5 a6a5 f5f6 a5a4 f6g7 a4a3 g7h8q"
                (Position parsed) = parse $ "position startpos moves " ++ movesWithPromotion in
                   parsed `shouldSatisfy` isRight
        it "parses `position` command with no moves" do
            parse "position startpos moves" `shouldBe` (Position $ Right initialPosition)

        it "parses `position` without `moves` part" do
            parse "position startpos" `shouldBe` (Position $ Right initialPosition)

        it "handles unexpected moves in `position` command" do
            let (Position parsed) = parse "position startpos e2e4 e7e5" in
                parsed `shouldSatisfy` isLeft

        it "handles invalid moves in `position` command" do
            let (Position parsed) = parse "position startpos moves e3e4 e7e5" in
                parsed `shouldSatisfy` isLeft

        it "handles invalid first part in `position` command" do
            let (Position parsed) = parse "position wrong moves e2e4 e7e5" in
                parsed `shouldSatisfy` isLeft

        it "handles bad files in `position` command" do
            let (Position parsed) = parse "position startpos moves k2e4" in
                parsed `shouldSatisfy` isLeft

        it "handles bad ranks in `position` command" do
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

        it "returns nothing on `position` command" do
            (Position $ Right initialPosition) `responseShouldBe` []

        it "returns bestmove on `go` command" do
            Go `responseShouldBe` ["bestmove e2e4"]

        it "returns unknown on unknown command" do
            Unknown "garbage" `responseShouldBe` ["unknown command garbage"]

        it "returns nothing on `quit` command" do
            Quit `responseShouldBe` []


describeUciPlay :: Spec
describeUciPlay = 
    describe "Uci.play" do
        it "plays a game" do
            responsesRef <- newIORef []
            commandsRef <- newIORef [Uci, IsReady, UciNewGame, 
                                     (Position $ Right initialPosition), 
                                     Go, Quit]

            play (commands commandsRef) Morphy (refWriter responsesRef)

            responses <- readIORef responsesRef
            responses `shouldBe` 
                [uciResponse, 
                 readyOkResponse, 
                 emptyResponse, 
                 emptyResponse, 
                 bestMoveResponse "e2e4"]


responseShouldBe :: Command -> [String] -> IO ()
command `responseShouldBe` expectedLines = do
    response <- Morphy `getResponseFor` command
    response `shouldBe` Response expectedLines


data Morphy = Morphy
instance Engine Morphy where
    _ `getResponseFor` Uci.Go = do
        return $ bestMoveResponse "e2e4"

    _ `getResponseFor` Uci.Uci = return uciResponse

    _ `getResponseFor` Uci.IsReady = return readyOkResponse

    _ `getResponseFor` Uci.UciNewGame = return emptyResponse

    _ `getResponseFor` (Uci.Position _) = return emptyResponse

    _ `getResponseFor` (Uci.Unknown s) = return $ unknownResponse s

    _ `getResponseFor` Uci.Quit = return emptyResponse


commands :: IORef [Command] -> IO Command
commands ref = do
    curCommands <- readIORef ref
    modifyIORef ref tail
    return $ head curCommands

    
refWriter :: IORef [Response] -> Response -> IO ()
refWriter ref response = do
    modifyIORef ref (++ [response])


positionAfterE4E5 :: Either String P.Position
positionAfterE4E5 = do
    afterE4 <- initialPosition `make` Move e2 e4 Nothing
    afterE5 <- afterE4 `make` Move e7 e5 Nothing
    return afterE5
