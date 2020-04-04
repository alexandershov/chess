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

type CommandReader = IO Command
type ResponseWriter = Response -> IO ()


play :: CommandReader -> Player -> ResponseWriter -> IO ()
play reader player writer = do
    command <- reader
    case command of
        Quit -> return ()
        _ -> do
            response <- getResponse command player
            writer response
            play reader player writer


turk :: FilePath -> Player
turk path = do
    handle <- openFile path ReadMode
    hGetLine handle


stdinReader :: FilePath -> CommandReader
stdinReader logPath = do
    line <- getLine
    logLine <- makeLogLine "command" line
    appendFile logPath logLine
    return $ parse line


stdoutWriter :: FilePath -> Response -> IO ()
stdoutWriter logPath (Response responseLines) = do
    logLines <- mapM (makeLogLine "response") responseLines
    mapM_ (appendFile logPath) logLines
    putStr $ unlines responseLines
    hFlush stdout


makeLogLine :: String -> String -> IO String
makeLogLine kind line = do
    currentTime <- getCurrentTime
    let entries = [(show currentTime),  kind, line] in
        return $ (intercalate "\t" entries) ++ "\n"


getResponse :: Command -> Player -> IO Response

getResponse Uci.Uci _ = return uciResponse

getResponse Uci.IsReady _ = return readyOkResponse

getResponse Uci.UciNewGame _ = return emptyResponse

getResponse Uci.Position _ = return emptyResponse

getResponse Uci.Go player = do
    move <- player
    return $ bestMoveResponse move

getResponse (Uci.Unknown _) _ = return emptyResponse

getResponse Uci.Quit _ = return emptyResponse


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
