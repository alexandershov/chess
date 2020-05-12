{-# LANGUAGE NamedFieldPuns #-}

module Repetition where

import Data.Map as M

import Position hiding(repetitions)
import qualified Position as P

getNextRepetitions :: Position -> M.Map Position Int
getNextRepetitions nextPosition'@Position{P.repetitions} =
    case M.lookup essence repetitions of
        Nothing -> M.insert essence 1 repetitions
        Just n -> M.insert essence (n + 1) repetitions
    where essence = nextPosition'{halfMoveClock=0, P.repetitions=M.empty}
