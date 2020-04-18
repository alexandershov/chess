module Main where

import Data.IORef
import System.Environment

import Uci

main :: IO ()
main = do
    args <- getArgs
    case args of
        [logPath] -> do
            positionRef <- newIORef Nothing
            play (stdinReader logPath) (Currychnoi positionRef) (stdoutWriter logPath)

        _ -> error "Usage: chess-exe log-path"
