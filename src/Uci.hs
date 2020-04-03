module Uci where

data Command = 
    Uci 
    | IsReady 
    | UciNewGame 
    | Unknown String deriving (Eq, Show)

parse :: String -> Command
parse "uci" = Uci
parse "isready" = IsReady
parse "ucinewgame" = UciNewGame
parse s = Unknown s