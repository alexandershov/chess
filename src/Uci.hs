module Uci where

data Input = Uci | ReadyOk deriving (Eq, Show)

parse :: String -> Input
parse "uci" = Uci