{-# LANGUAGE BlockArguments #-}

module Uci where

import Data.List
import Data.Time (getCurrentTime)
import System.IO

data Command = 
    Uci |
    IsReady |
    UciNewGame |
    Position |
    Go |
    Quit |
    Unknown String deriving (Eq, Show)

data Response = Response [String] deriving (Eq, Show)
type Player = IO String

findBestMove :: Player -> Player
findBestMove = id

type CommandReader = IO Command
type ResponseWriter = Response -> IO ()


play :: CommandReader -> Player -> ResponseWriter -> IO ()
play readCommand player writeResponse = do
    command <- readCommand
    case command of
        Quit -> return ()
        _ -> do
            response <- getResponse player command
            writeResponse response
            play readCommand player writeResponse


turk :: FilePath -> Player
turk path = do
    handle <- openFile path ReadMode
    hGetLine handle


readCommandWithLog :: FilePath -> CommandReader
readCommandWithLog logPath = do
    line <- getLine
    logLine <- makeLogLine "command" line
    appendFile logPath logLine
    return $ parse line


writeResponseWithLog :: FilePath -> Response -> IO ()
writeResponseWithLog logPath (Response responseLines) = do
    logLines <- mapM (makeLogLine "response") responseLines
    mapM_ (appendFile logPath) logLines
    putStr $ unlines responseLines
    hFlush stdout


makeLogLine :: String -> String -> IO String
makeLogLine kind line = do
    currentTime <- getCurrentTime
    let entries = [(show currentTime),  kind, line] in
        return $ (intercalate "\t" entries) ++ "\n"


getResponse :: Player -> Command -> IO Response

getResponse _ Uci.Uci = return uciResponse

getResponse _ Uci.IsReady = return readyOkResponse

getResponse _ Uci.UciNewGame = return emptyResponse

getResponse _ Uci.Position = return emptyResponse

getResponse player Uci.Go = do
    move <- findBestMove player
    return $ bestMoveResponse move

getResponse _ _ = error "TODO: remove this"


uciResponse :: Response
uciResponse = Response ["id name chess", "id author Alexander Ershov", "uciok"]

readyOkResponse :: Response
readyOkResponse = Response ["readyok"]

emptyResponse :: Response
emptyResponse = Response []

bestMoveResponse :: String -> Response
bestMoveResponse move = Response ["bestmove " ++ move]


parse :: String -> Command
parse "uci" = Uci
parse "isready" = IsReady
parse "ucinewgame" = UciNewGame
parse "quit" = Quit
parse s =
    case words s of
        "position":_ -> Position
        "go":_ -> Go
        _ -> Unknown s
