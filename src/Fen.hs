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


parseRankElement :: Char -> Either String [Maybe Piece]
parseRankElement 'R' = Right [Just whiteRook]
parseRankElement 'N' = Right [Just whiteKnight]
parseRankElement 'B' = Right [Just whiteBishop]
parseRankElement 'Q' = Right [Just whiteQueen]
parseRankElement 'K' = Right [Just whiteKing]
parseRankElement 'P' = Right [Just whitePawn]

parseRankElement 'r' = Right [Just blackRook]
parseRankElement 'n' = Right [Just blackKnight]
parseRankElement 'b' = Right [Just blackBishop]
parseRankElement 'q' = Right [Just blackQueen]
parseRankElement 'k' = Right [Just blackKing]
parseRankElement 'p' = Right [Just blackPawn]

parseRankElement s = do
    n <- readEither [s]
    if n < 1 || n > 8 
        then Left $ "n should be in [0..8], got " ++ [s]
        else return $ take n (repeat Nothing) 
