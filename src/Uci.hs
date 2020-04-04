module Uci where

data Command = 
    Uci |
    IsReady |
    UciNewGame |
    Position |
    Go |
    Unknown String deriving (Eq, Show)


data Response = Response [String]

parse :: String -> Command
parse "uci" = Uci
parse "isready" = IsReady
parse "ucinewgame" = UciNewGame
parse s =
    case words s of
        "position":_ -> Position
        "go":_ -> Go
        _ -> Unknown s


class CommandReader a where
    read :: a -> IO Command

class ResponseWriter a where
    write :: a -> Response -> IO ()

class Player a where
    findBestMove :: a -> IO String