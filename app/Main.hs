module Main where

import System.Environment
import Uci

main :: IO ()
main = do
    args <- getArgs
    case args of
        [logPath, turkPath] -> play (readCommand logPath) (turk turkPath) (writeResponse logPath)
        _ -> error "Usage: chess-exe log-path turk-path"
