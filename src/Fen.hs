module Fen where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)
import Text.Read (readEither)

import Pieces
import Position hiding (castlingRights)
import Squares

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


parseSideToMove :: String -> Either String Color
parseSideToMove "w" = Right White
parseSideToMove "b" = Right Black
parseSideToMove s = Left $ "side to move should either `w` or `b`, got " ++ s


parseCastlingRights :: String -> Either String CastlingRights
parseCastlingRights "-" = Right M.empty
parseCastlingRights s =
    if allUnique 
        then combineRights rights
        else Left $ "got duplicates in castling rights " ++ s
    where rights = map parseOneCastlingRight s
          allUnique = length (nub rights) == length rights


combineRights :: [Either String (Color, Castle)] -> Either String CastlingRights
combineRights [] = Right M.empty
combineRights (x:xs) = do
    (color, castle) <- x
    result <- combineRights xs
    return $ with result color castle


with :: CastlingRights -> Color -> Castle -> CastlingRights
with castlingRights color castle =
    M.insert color newCastles castlingRights
    where curCastles = maybe S.empty id (M.lookup color castlingRights)
          newCastles = S.insert castle curCastles


parseOneCastlingRight :: Char -> Either String (Color, Castle)
parseOneCastlingRight 'K' = Right (White, ShortCastle)
parseOneCastlingRight 'k' = Right (Black, ShortCastle)
parseOneCastlingRight 'Q' = Right (White, LongCastle)
parseOneCastlingRight 'q' = Right (Black, LongCastle)
parseOneCastlingRight s = Left $ "castling rights should be one of `KkQq`, got " ++ [s]


parseEnPassant :: String -> Either String (Maybe Square)
parseEnPassant [file, rank] = do
    square <- parseSquare file rank
    return $ Just square
parseEnPassant "-" = Right Nothing
parseEnPassant s = Left $ "en passant should be a valid square, got " ++ s


parseHalfMoveClock :: String -> Either String Int
parseHalfMoveClock s = readEither s
