{-# LANGUAGE BlockArguments #-}

module Uci (
    Command(..),
    Response(..),
    play,
    parse,
    getResponse,
    Player(..),
    CommandReader(..),
    ResponseWriter(..)
) where

data Command = 
    Uci |
    IsReady |
    UciNewGame |
    Position |
    Go |
    Quit |
    Unknown String deriving (Eq, Show)

data Response = Response [String] deriving (Eq, Show)


play :: (CommandReader a) => (Player b) => (ResponseWriter c) => a -> b -> c -> IO ()
play reader player writer = do
    command <- Uci.read reader
    case command of
        Quit -> return ()
        _ -> do
            response <- getResponse player command
            write writer response
            play reader player writer


getResponse :: (Player a) => a -> Command -> IO Response

getResponse _ Uci.Uci = return idResponse

getResponse _ Uci.IsReady = return readyOkResponse

getResponse _ Uci.UciNewGame = return emptyResponse

getResponse _ Uci.Position = return emptyResponse

getResponse player Uci.Go = do
    move <- findBestMove player
    return $ bestMoveResponse move

getResponse _ _ = error "TODO: remove this"


idResponse :: Response
idResponse = Response ["id name chess", "id author Alexander Ershov"]

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
parse s =
    case words s of
        "position":_ -> Position
        "go":_ -> Go
        _ -> Unknown s


class Player a where
    findBestMove :: a -> IO String


class CommandReader a where
    read :: a -> IO Command


class ResponseWriter a where
    write :: a -> Response -> IO ()