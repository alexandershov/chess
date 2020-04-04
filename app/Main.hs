module Main where

import System.Environment
import Uci

main :: IO ()
main = do
    args <- getArgs
    case args of
        [logPath, turkPath] -> play (readCommandWithLog logPath) (turk turkPath) (writeResponseWithLog logPath)
        _ -> error "Usage: chess-exe turk-path log-path"
