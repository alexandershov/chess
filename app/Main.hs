module Main where

import System.Environment
import System.IO

import Uci

main :: IO ()
main = do
    args <- getArgs
    case args of
        [logPath, turkPath] -> do
            turkHandle <- openFile turkPath ReadMode
            play (stdinReader logPath) (Turk turkHandle) (stdoutWriter logPath)

        _ -> error "Usage: chess-exe log-path turk-path"
