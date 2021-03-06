module Fen where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Array (array)
import Data.List (nub)
import Data.List.Split
import Text.Read (readEither)

import Moves
import Pieces
import qualified Position as P
import Position hiding (board, sideToMove, castlingRights, enPassant, halfMoveClock)
import Squares


parsePosition :: String -> Either String Position
parsePosition s = 
    case splitOn " " s of
        [board', sideToMove', castlingRights', enPassant', halfMoveClock', _] -> do
            board <- parseBoard board'
            sideToMove <- parseSideToMove sideToMove'
            castlingRights <- parseCastlingRights castlingRights'
            enPassant <- parseEnPassant enPassant'
            halfMoveClock <- parseHalfMoveClock halfMoveClock'
            return $ withRepetition (Position board sideToMove castlingRights enPassant halfMoveClock M.empty)
        parts -> Left $ "fen " ++ s ++ " should have 6 parts, got " ++ (show $ length parts)


parseBoard :: String -> Either String Board
parseBoard board' = 
    if length ranks /= boardSize
        then Left $ "there should be " ++ (show boardSize) ++ " ranks, got " ++ board'
        else do
            nested <- sequence $ map parseRankLine ranksWithIndexes
            return $ array (a1, h8) (concat nested)
    where ranks = splitOn "/" board'
          ranksWithIndexes = zip (reverse [1..boardSize]) ranks
          

parseRankLine :: (Int, String) -> Either String [(Square, Maybe Piece)]
parseRankLine (rank, s) = do
    nestedElements <- sequence elements'
    let elements = concat nestedElements in
        if length elements /= boardSize
            then Left $ invalidRankLine s elements
            else return $ [((file, rank), element) | (file, element) <- (zip files elements)]
    where elements' = map parseRankElement s
          files = [1..boardSize]


invalidRankLine :: String -> [Maybe Piece] -> String
invalidRankLine s elements = 
    "rank " ++ s ++ " should have " ++ (show boardSize) ++ " elements " ++ " got " ++ (show (length elements))


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
        then Left $ "n should be in [1..8], got " ++ [s]
        else return $ replicate n Nothing


withRepetition :: Position -> Position
withRepetition position = 
    position{P.repetitions=getNextRepetitions position} 


parseSideToMove :: String -> Either String Color
parseSideToMove "w" = Right White
parseSideToMove "b" = Right Black
parseSideToMove s = Left $ "side to move should either `w` or `b`, got " ++ s


parseCastlingRights :: String -> Either String CastlingRights
parseCastlingRights "-" = Right noCastlingRights
parseCastlingRights s =
    if allUnique 
        then combineRights rights
        else Left $ "got duplicates in castling rights " ++ s
    where rights = map parseOneCastlingRight s
          allUnique = length (nub rights) == length rights


combineRights :: [Either String (Color, Castle)] -> Either String CastlingRights
combineRights xs = do
    rights <- sequence xs
    return $ foldl with noCastlingRights rights


with :: CastlingRights -> (Color, Castle) -> CastlingRights
with castlingRights (color, castle) =
    M.insert color newCastles castlingRights
    where Just curCastles = M.lookup color castlingRights
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
parseEnPassant s = Left $ "en passant should be a valid square or -, got " ++ s


parseHalfMoveClock :: String -> Either String Int
parseHalfMoveClock s = readEither s
