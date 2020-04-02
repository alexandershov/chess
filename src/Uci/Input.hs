module Uci.Input where

data Input = Uci | IsReady | Unknown String deriving (Eq, Show)

parse :: String -> Input
parse "uci" = Uci
parse "isready" = IsReady
parse s = Unknown s