module Pieces where

data Color = Black | White deriving (Eq, Show)
data Piece = 
    Pawn { getColor :: Color } | 
    Knight { getColor :: Color } | 
    Bishop { getColor :: Color } |
    Rook { getColor :: Color } | 
    Queen { getColor :: Color } | 
    King { getColor :: Color } deriving (Eq, Show)

whitePawn :: Piece
whitePawn = Pawn White

whiteKnight :: Piece
whiteKnight = Knight White

whiteBishop :: Piece
whiteBishop = Bishop White

whiteRook :: Piece
whiteRook = Rook White

whiteQueen :: Piece
whiteQueen = Queen White

whiteKing :: Piece
whiteKing = King White


blackPawn :: Piece
blackPawn = Pawn Black

blackKnight :: Piece
blackKnight = Knight Black

blackBishop :: Piece
blackBishop = Bishop Black

blackRook :: Piece
blackRook = Rook Black

blackQueen :: Piece
blackQueen = Queen Black

blackKing :: Piece
blackKing = King Black
