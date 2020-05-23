{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module MovesSpec where

import Data.Array ((!), elems)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either (isLeft)
import Data.Maybe (isJust)

import Test.Hspec

import Fen
import Moves
import Pieces
import Position hiding (board, sideToMove, castlingRights, enPassant, halfMoveClock)
import qualified Position as P
import Squares

describePieces :: Spec
describePieces = do
    describeWhitePawn
    describeBlackPawn
    describeKnight
    describeBishop
    describeRook
    describeQueen
    describeKing

    describeLegalMoves
    describeHalfMoveClock
    describeDraw
    describeDrawEnPassant

    describeWhiteCastlingRights
    describeBlackCastlingRights
    describeShortCastle
    describeLongCastle
    describeCastleMoves

    describePerft


describePerft :: Spec
describePerft = do
    mapM_ describePerftPosition cases
    where cases = [("initialPosition perft", initialPosition, [1, 20, 400, 8902, 197281]),
                   ("position #2 perft", two, [1, 48, 2039, 97862]),
                   ("position #3 perft", three, [1, 14, 191, 2812, 43238]),
                   ("position #4 perft", four, [1, 6, 264, 9467]),
                   ("position #5 perft", five, [1, 44, 1486, 62379]),
                   ("position #6 peft", six, [1, 46, 2079, 89890])]
          Right two = parsePosition "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
          Right three = parsePosition "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
          Right four = parsePosition "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
          Right five = parsePosition "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
          Right six = parsePosition "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"


describePerftPosition :: (String, Position, [Integer]) -> Spec
describePerftPosition (description, position, expected) = do
    describe description do
        mapM_ (describePerftAtDepth position) withIndexes
    where withIndexes = zip [0..] expected 


describePerftAtDepth :: Position -> (Integer, Integer) -> Spec
describePerftAtDepth position (depth, expected) = do
    it ("is correct at depth " ++ (show depth)) do
        perft position depth `shouldBe` expected


perft :: Position -> Integer -> Integer
perft _ 0 = 1
perft position depth = do
    sum $ [ perft p (depth - 1) | p <- positions ]
    where moves = legalMoves position
          positions = map (position `makeUnchecked`) moves


describeWhitePawn :: Spec
describeWhitePawn = do
    describe "White pawn" do
        it "moves forward by 1 square" do
            allMovesFrom e3 positionWithWhitePawn `shouldMatchList` [move' e3 e4]
        it "moves forward by 2 squares from the initial position" do
            allMovesFrom d2 positionWithWhitePawn `shouldMatchList` [move' d2 d3, move' d2 d4]
        it "stops at own piece" do
            allMovesFrom c4 positionWithWhitePawn `shouldMatchList` []
        it "stops at enemy piece" do
            allMovesFrom f4 positionWithWhitePawn `shouldMatchList` []
        it "captures enemy pieces" do
            allMovesFrom g2 positionWithWhitePawn `shouldMatchList` [move' g2 h3, move' g2 f3]
        it "doesn't capture own pieces" do
            allMovesFrom d2 positionWithWhitePawn `shouldMatchList` [move' d2 d3, move' d2 d4]
        it "promotes from the 7th rank" do
            allMovesFrom h7 positionWithWhitePawn `shouldMatchList` [
                Move h7 h8 (Just whiteKnight), Move h7 h8 (Just whiteBishop),
                Move h7 h8 (Just whiteRook), Move h7 h8 (Just whiteQueen)]
        it "takes en passant" do
            allMovesFrom e5 positionWithEnPassant `shouldMatchList` [move' e5 d6]


describeBlackPawn :: Spec
describeBlackPawn = do
    describe "Black pawn" do
        it "moves forward by 1 square" do
            allMovesFrom e6 positionWithBlackPawn `shouldMatchList` [move' e6 e5]
        it "moves forward by 2 squares from the initial position" do
            allMovesFrom d7 positionWithBlackPawn `shouldMatchList` [move' d7 d6, move' d7 d5]
        it "stops at own piece" do
            allMovesFrom c5 positionWithBlackPawn `shouldMatchList` []
        it "stops at enemy piece" do
            allMovesFrom f5 positionWithBlackPawn `shouldMatchList` []
        it "captures enemy pieces" do
            allMovesFrom g7 positionWithBlackPawn `shouldMatchList` [move' g7 h6, move' g7 f6]
        it "doesn't capture own pieces" do
            allMovesFrom d7 positionWithBlackPawn `shouldMatchList` [move' d7 d6, move' d7 d5]
        it "promotes from the 7th rank" do
            allMovesFrom h2 positionWithBlackPawn `shouldMatchList` [
                Move h2 h1 (Just blackKnight), Move h2 h1 (Just blackBishop),
                Move h2 h1 (Just blackRook), Move h2 h1 (Just blackQueen)]


describeKnight :: Spec
describeKnight = do
    describe "Knight" do
        it "jumps" do
            allMovesFrom f3 positionWithKnight `shouldMatchList` knightF3Moves


describeBishop :: Spec
describeBishop = do
    describe "Bishop" do
        it "moves by diagonals" do
            allMovesFrom b2 positionWithBishop `shouldMatchList` bishopB2Moves


describeRook :: Spec
describeRook = do
    describe "Rook" do
        it "moves by straight lines" do
            allMovesFrom b2 positionWithRook `shouldMatchList` rookB2Moves


describeQueen :: Spec
describeQueen = do
    describe "Queen" do
        it "moves by diagonals and straight lines" do
            allMovesFrom b2 positionWithQueen `shouldMatchList` queenB2Moves


describeKing :: Spec
describeKing = do
    describe "King" do
        it "moves one square in each direction" do
            allMovesFrom e1 positionWithKing `shouldMatchList` kingE1Moves


describeInitialPosition :: Spec
describeInitialPosition = do
    describe "Initial position" do
        it "has 32 pieces" do
            length pieces `shouldBe` 32
        it "has white as side to move" do
            sideToMove `shouldBe` White
        it "allows every castle" do
            castlingRights `shouldBe` M.fromList [(White, bothCastles), (Black, bothCastles)]
        it "has no en passant square" do
            enPassant `shouldBe` Nothing
        it "has half move clock set to zero" do
            halfMoveClock `shouldBe` 0
        where Position{P.board, P.sideToMove, P.castlingRights, P.enPassant, P.halfMoveClock} = initialPosition
              pieces = filter isJust $ elems board


describeMakeMove :: Spec
describeMakeMove = do
    describe "Making a move" do
        it "moves a piece" do
            (board ! g1) `shouldBe` Nothing
            (board ! f3) `shouldBe` Just whiteKnight
        it "promotes a pawn" do
            (boardAfterPromotion ! h7) `shouldBe` Nothing
            (boardAfterPromotion ! h8) `shouldBe` Just whiteQueen
        it "changes a side to move" do
            sideToMove `shouldBe` Black
        it "returns Left if move is illegal" do
            initialPosition `make` move' f3 g1 `shouldSatisfy` isLeft
        it "by pawn sets en passant if it is a double move" do
            enPassantAfterDoublePawnMove `shouldBe` Just d6
        it "by pawn doesn't sets en passant if it is a single move" do
            enPassantAfterSinglePawnMove `shouldBe` Nothing
        it "en passant is possible" do
            boardAfterEnPassant ! e5 `shouldBe` Nothing
            boardAfterEnPassant ! d6 `shouldBe` Just whitePawn
            boardAfterEnPassant ! d5 `shouldBe` Nothing
    where Right Position{P.board, P.sideToMove} = initialPosition `make` move' g1 f3
          Position{P.enPassant=enPassantAfterDoublePawnMove} = positionWithEnPassant
          Right Position{P.board=boardAfterEnPassant} = positionWithEnPassant `make` move' e5 d6
          Right Position{P.enPassant=enPassantAfterSinglePawnMove} = initialPosition `make` move' e2 e3
          Right Position{P.board=boardAfterPromotion} = positionWithWhitePawn `make` Move h7 h8 (Just whiteQueen)


describeWhiteCastlingRights :: Spec
describeWhiteCastlingRights = do
    describe "White castle" do
        it "right is lost after the queen rook moves" do
            castlingRightsAfter (move' a1 a2) position `shouldMatchList` [
                (White, [ShortCastle]), (Black, [LongCastle, ShortCastle])]
        it "right is lost after the king rook moves" do
            castlingRightsAfter (move' h1 h2) position `shouldMatchList` [
                (White, [LongCastle]), (Black, [LongCastle, ShortCastle])]
        it "right is lost after the white king moves" do
            castlingRightsAfter (move' e1 e2) position `shouldMatchList` [
                (White, []), (Black, [LongCastle, ShortCastle])]
        it "right is not lost after the third rook moves" do
            castlingRightsAfter (move' c4 c5) position `shouldMatchList` [
                (White, [LongCastle, ShortCastle]), (Black, [LongCastle, ShortCastle])]
        it "right is lost after the queen rook is taken" do
            castlingRightsAfter (move' a8 a1) blackPosition `shouldMatchList` [
                (White, [ShortCastle]), (Black, [ShortCastle])]
        it "right is lost after the king rook is taken" do
            castlingRightsAfter (move' h8 h1) blackPosition `shouldMatchList` [
                (White, [LongCastle]), (Black, [LongCastle])]
    where position = positionWithCastling White
          blackPosition = positionWithCastling Black


describeBlackCastlingRights :: Spec
describeBlackCastlingRights = do
    describe "Black castle" do
        it "right is lost after the queen rook moves" do
            castlingRightsAfter (move' a8 a7) position `shouldMatchList` [
                (Black, [ShortCastle]), (White, [LongCastle, ShortCastle])]
        it "right is lost after the king rook moves" do
            castlingRightsAfter (move' h8 h7) position `shouldMatchList` [
                (Black, [LongCastle]), (White, [LongCastle, ShortCastle])]
        it "right is lost after the white king moves" do
            castlingRightsAfter (move' e8 e7) position `shouldMatchList` [
                (Black, []), (White, [LongCastle, ShortCastle])]
        it "right is not lost after the third rook moves" do
            castlingRightsAfter (move' c5 c4) position `shouldMatchList` [
                (Black, [LongCastle, ShortCastle]), (White, [LongCastle, ShortCastle])]
        it "right is lost after the queen rook is taken" do
            castlingRightsAfter (move' a1 a8) whitePosition `shouldMatchList` [
                (Black, [ShortCastle]), (White, [ShortCastle])]
        it "right is lost after the king rook is taken" do
            castlingRightsAfter (move' h1 h8) whitePosition `shouldMatchList` [
                (Black, [LongCastle]), (White, [LongCastle])]
    where position = positionWithCastling Black
          whitePosition = positionWithCastling White


describeShortCastle :: Spec
describeShortCastle = do
    describe "Short castle" do
        it "moves a king" do
            board ! g1 `shouldBe` Just whiteKing
            board ! e1 `shouldBe` Nothing
        it "moves a rook" do
            board ! f1 `shouldBe` Just whiteRook
            board ! h1 `shouldBe` Nothing
        it "changes side to move" do
            sideToMove `shouldBe` Black
        it "removes castling rights" do
            convertCastlingRights castlingRights `shouldMatchList` [(White, []), (Black, [LongCastle, ShortCastle])]
    where position = positionWithCastling White
          Right Position{P.board, P.sideToMove, P.castlingRights} = position `make` move' e1 g1


describeLongCastle :: Spec
describeLongCastle = do
    describe "Long castle" do
        it "moves a king" do
            board ! c1 `shouldBe` Just whiteKing
            board ! e1 `shouldBe` Nothing
        it "moves a rook" do
            board ! d1 `shouldBe` Just whiteRook
            board ! a1 `shouldBe` Nothing
        it "changes side to move" do
            sideToMove `shouldBe` Black
        it "removes castling rights" do
            convertCastlingRights castlingRights `shouldMatchList` [(White, []), (Black, [LongCastle, ShortCastle])]
    where position = positionWithCastling White
          Right Position{P.board, P.sideToMove, P.castlingRights} = position `make` move' e1 c1


describeCastleMoves :: Spec
describeCastleMoves = do
    describe "Long and short castle" do
        it "are possible moves" do
            allMovesFrom e1 (positionWithCastling White) `shouldMatchList` (kingMoves ++ castleMoves)
        it "are not possible if there's no right to castle" do
            allMovesFrom e1 (positionWithoutCastling White) `shouldMatchList` kingMoves
        it "are not possible if there's a piece obstacle" do
            allMovesFrom e1 positionWithObstacle `shouldMatchList` kingMoves
        it "are not possible if there's a threat on the king path" do
            allMovesFrom e1 positionWithThreat `shouldMatchList` kingMoves
    where kingMoves = [move' e1 d1, move' e1 f1, move' e1 d2, move' e1 e2, move' e1 f2]
          castleMoves = [move' e1 g1, move' e1 c1]
          positionWithObstacle = (positionWithCastling White) `changeBoard` [
              whiteKnight `on` b1, whiteKnight `on` g1]
          positionWithThreat = (positionWithCastling White) `changeBoard` [blackQueen `on` d3]


castlingRightsAfter :: Move -> Position -> [(Color, [Castle])]
castlingRightsAfter move position =
    convertCastlingRights castlingRights
    where Right Position{P.castlingRights} = position `make` move


convertCastlingRights :: CastlingRights -> [(Color, [Castle])]
convertCastlingRights castlingRights =
    [(color, S.toList castles) | (color, castles) <- M.toList castlingRights]


describeLegalMoves :: Spec
describeLegalMoves = do
    describe "Legal move" do
        it "doesn't leave king under attack" do
            legalMoves positionWithCheck `shouldMatchList` [
                move' e1 f1, move' e1 f2, move' e1 d1, move' e1 d2,
                move' b5 e8, move' b5 e2]


describeHalfMoveClock :: Spec
describeHalfMoveClock = do
    describe "Half move clock" do
        it "increases with a piece move without capture" do
            clockAfterNf3 `shouldBe` 1
        it "resets after a pawn move" do
            clockAfterE5 `shouldBe` 0
        it "resets after a capture" do
            clockAfterNh4 `shouldBe` 0
    where Right afterNf3@Position{P.halfMoveClock=clockAfterNf3} = initialPosition `make` move' g1 f3
          Right afterE5@Position{P.halfMoveClock=clockAfterE5} = afterNf3 `make` move' e7 e5
          Right afterE3 = afterE5 `make` move' e2 e3
          Right afterQh4 = afterE3 `make` move' d8 h4
          Right Position{P.halfMoveClock=clockAfterNh4} = afterQh4 `make` move' f3 h4


describeDraw :: Spec
describeDraw = do
    describe "Draw" do
        it "can't  be claimed if half move clock is < 100" do
            isDraw initialPosition `shouldBe` False
            isDraw initialPosition{P.halfMoveClock=99} `shouldBe` False

        it "can be claimed if half move clock is >= 100" do
            isDraw initialPosition{P.halfMoveClock=100} `shouldBe` True

        it "can't be claimed after twofold repetition" do
            isDraw twofold `shouldBe` False

        it "can be claimed after threefold repetition" do
            isDraw threefold `shouldBe` True

        it "can't be claimed if side to move is different" do
            isDraw wrongThreefold `shouldBe` False

    where moves = cycle [move' g1 f3, move' g8 f6, move' f3 g1, move' f6 g8]
          wrongThreefoldMoves = [move' e2 e3, move' g8 f6, move' f1 e2, move' f6 g8,
                                 move' e2 f1, move' g8 f6, move' f1 e2, move' f6 g8,
                                 move' e2 c4, move' g8 f6, move' c4 d3, move' f6 g8,
                                 move' d3 e2, move' g8 f6]
          twofold = initialPosition `makeUncheckedMoves` (take 4 moves)
          threefold = initialPosition `makeUncheckedMoves` (take 8 moves)
          wrongThreefold = initialPosition `makeUncheckedMoves` wrongThreefoldMoves


describeDrawEnPassant :: Spec
describeDrawEnPassant = do
    describe "Draw" do
        it "by threefold repetition can be claimed if en passant is not possible" do
            isDraw impossibleThreeFold `shouldBe` True
        it "by threefold repetition can't be claimed if en passant is possible" do
            isDraw possibleTwoFold `shouldBe` False
    where moves = [move' f2 f4] ++ cycle [move' g8 g7, move' g1 f1, move' g7 g8, move' f1 g1]
          Right impossible = parsePosition "6k1/8/8/8/6p1/8/5PR1/6K1 w - - 0 1"
          Right possible = parsePosition "6k1/8/8/8/6p1/8/R4P2/6K1 w - - 0 1"
          impossibleThreeFold = impossible `makeUncheckedMoves` (take 9 moves)
          possibleTwoFold = possible `makeUncheckedMoves` (take 9 moves)

          
allMovesFrom :: Square -> Position -> [Move]
allMovesFrom square position =
    filter (`isFrom` square) $ allMoves position


isFrom :: Move -> Square -> Bool
(Move from _ _) `isFrom` square = from == square


positionWithCastling :: Color -> Position
positionWithCastling color =
    Position board color fullCastlingRights Nothing 0 M.empty
    where board = boardWithKingsAndRooks


positionWithoutCastling :: Color -> Position
positionWithoutCastling color =
    Position board color noCastlingRights Nothing 0 M.empty
    where board = boardWithKingsAndRooks


boardWithKingsAndRooks :: Board
boardWithKingsAndRooks = 
    put [whiteRook `on` a1, whiteKing `on` e1, whiteRook `on` h1,
         whiteRook `on` c4, blackRook `on` c5,
         blackRook `on` a8, blackKing `on` e8, blackRook `on` h8]


positionWithWhitePawn :: Position
positionWithWhitePawn = 
    Position board White noCastlingRights Nothing 0 M.empty
    where board = put [whitePawn `on` e3, whitePawn `on` d2,
                       whitePawn `on` c4, whitePawn `on` c5,
                       whitePawn `on` f4, blackPawn `on` f5,
                       whitePawn `on` g2, blackPawn `on` g3,
                       whitePawn `on` h7,
                       blackKnight `on` f3, blackBishop `on` h3]


positionWithEnPassant :: Position
positionWithEnPassant =
    makeUncheckedMoves initialPosition [move' e2 e4, move' e7 e6, move' e4 e5, move' d7 d5]


makeUncheckedMoves :: Position -> [Move] -> Position
makeUncheckedMoves position moves = foldl makeUnchecked position moves


positionWithBlackPawn :: Position
positionWithBlackPawn = 
    Position board Black noCastlingRights Nothing 0 M.empty
    where board = put [blackPawn `on` e6, blackPawn `on` d7,
                       blackPawn `on` c5, blackPawn `on` c4,
                       blackPawn `on` f5, whitePawn `on` f4,
                       blackPawn `on` g7, whitePawn `on` g6,
                       blackPawn `on` h2,
                       whiteKnight `on` f6, whiteBishop `on` h6]


positionWithKnight :: Position
positionWithKnight = 
    Position board White noCastlingRights Nothing 0 M.empty
    where board = put [whiteKnight `on` f3, blackPawn `on` e5, 
                       whiteRook `on` e1, whiteBishop `on` f2]


positionWithBishop :: Position
positionWithBishop = 
    Position board White noCastlingRights Nothing 0 M.empty
    where board = put [whiteBishop `on` b2, blackPawn `on` e5, 
                       whiteKnight `on` c1]


positionWithRook :: Position
positionWithRook =
    Position board White noCastlingRights Nothing 0 M.empty
    where board = put [whiteRook `on` b2, whiteKing `on` e2, 
                       blackKnight `on` b5]


positionWithQueen :: Position
positionWithQueen =
    Position board White noCastlingRights Nothing 0 M.empty
    where board = put [whiteQueen `on` b2, whiteKing `on` e2, 
                       blackKnight `on` b5, blackPawn `on` e5,
                       whiteKnight `on` c1]


positionWithKing :: Position
positionWithKing =
    Position board White noCastlingRights Nothing 0 M.empty
    where board = put [whiteKing `on` e1, blackPawn `on` e2, 
                       whiteKnight `on` d2]


positionWithCheck :: Position
positionWithCheck = 
    Position board White noCastlingRights Nothing 0 M.empty
    where board = put [whiteKing `on` e1, blackRook `on` e8,
                       whiteBishop `on` b5]


knightF3Moves :: [Move]
knightF3Moves = [move' f3 e5, move' f3 g5, move' f3 h4, move' f3 h2, move' f3 g1,
                 move' f3 d2, move' f3 d4]


bishopB2Moves :: [Move]
bishopB2Moves = [move' b2 a1, move' b2 a3, move' b2 c3, move' b2 d4, move' b2 e5]


rookB2Moves :: [Move]
rookB2Moves = [move' b2 b3, move' b2 b4, move' b2 b5, move' b2 b1,
               move' b2 a2, move' b2 c2, move' b2 d2]


queenB2Moves :: [Move]
queenB2Moves = bishopB2Moves ++ rookB2Moves


kingE1Moves :: [Move]
kingE1Moves = [move' e1 f1, move' e1 d1, move' e1 e2, move' e1 f2]


changeBoard :: Position -> [(Piece, Square)] -> Position
position@Position{P.board} `changeBoard` piecesOnSquares =
    position {P.board=newBoard}
    where newBoard = (putOnBoard board piecesOnSquares)

move' :: Square -> Square -> Move
move' from to = Move from to Nothing
