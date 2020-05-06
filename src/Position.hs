{-# LANGUAGE NamedFieldPuns #-}

module Position where

import Data.Array (Array)
import qualified Data.Set as S
import qualified Data.Map as M

import Pieces
import Squares

type Board = Array Square (Maybe Piece)
data Castle = LongCastle | ShortCastle deriving (Eq, Ord, Show)
type CastlingRights = M.Map Color (S.Set Castle)

data Position = Position {board :: Board, 
                          sideToMove :: Color,
                          castlingRights :: CastlingRights,
                          enPassant :: (Maybe Square),
                          halfMoveClock :: Int} deriving (Eq, Ord, Show)


isDraw :: Position -> Bool
isDraw Position{halfMoveClock} =
    halfMoveClock >= 100