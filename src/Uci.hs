module Uci where

data Command = 
    Uci 
    | IsReady 
    | UciNewGame 
    | Position
    | Unknown String deriving (Eq, Show)

parse :: String -> Command
parse "uci" = Uci
parse "isready" = IsReady
parse "ucinewgame" = UciNewGame
parse s =
    case words s of
        "position":_ -> Position
        _ -> Unknown s
