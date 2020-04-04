module Main where

import System.Environment
import Uci

main :: IO ()
main = do
    args <- getArgs
    case args of
        [logPath, turkPath] -> play (stdinReader logPath) (turk turkPath) (stdoutWriter logPath)
        _ -> error "Usage: chess-exe log-path turk-path"
