module Uci (
    Command(..),
    Response(..),
    parse,
    getResponse,
    Player(..)
) where

data Command = 
    Uci |
    IsReady |
    UciNewGame |
    Position |
    Go |
    Unknown String deriving (Eq, Show)

data Response = Response [String] deriving (Eq, Show)


getResponse :: (Player a) => a -> Command -> IO Response

getResponse _ Uci.Uci = return uciResponse

getResponse _ Uci.IsReady = return readyOkResponse

getResponse _ Uci.UciNewGame = return emptyResponse

getResponse _ Uci.Position = return emptyResponse

getResponse player Uci.Go = do
    move <- findBestMove player
    return $ bestMoveResponse move

getResponse _ _ = error "TODO: remove this"


uciResponse :: Response
uciResponse = Response ["id name chess", "id author Alexander Ershov"]

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