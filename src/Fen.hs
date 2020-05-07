module Fen where

import Text.Read (readEither)

import Pieces
import Squares

parseSideToMove :: String -> Either String Color
parseSideToMove "w" = Right White
parseSideToMove "b" = Right Black
parseSideToMove s = Left $ "side to move should either `w` or `b`, got " ++ s


parseHalfMoveClock :: String -> Either String Int
parseHalfMoveClock s = readEither s

parseEnPassant :: String -> Either String (Maybe Square)
parseEnPassant [file, rank] = do
    square <- parseSquare file rank
    return $ Just square
parseEnPassant "-" = Right Nothing
parseEnPassant s = Left $ "en passant should be a valid square, got " ++ s