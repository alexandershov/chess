module Fen where

import Text.Read (readEither)

import Pieces

type ErrorDesc = String

parseSideToMove :: String -> Either ErrorDesc Color
parseSideToMove "w" = Right White
parseSideToMove "b" = Right Black
parseSideToMove s = Left $ "side to move should either `w` or `b`, got " ++ s


parseHalfMoveClock :: String -> Either ErrorDesc Int
parseHalfMoveClock s = readEither s