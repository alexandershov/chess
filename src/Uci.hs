module Uci where

data Command = Uci | IsReady | Unknown String deriving (Eq, Show)

parse :: String -> Command
parse "uci" = Uci
parse "isready" = IsReady
parse s = Unknown s