{-# LANGUAGE BlockArguments #-}

module Uci where

import Data.IORef
import Data.List
import Data.Time (getCurrentTime)
import System.IO
import System.Random

import Engine hiding (position)
import Moves
import Pieces
import qualified Position as P
import Squares

data Command = 
    Uci |
    IsReady |
    UciNewGame |
    Position (Either String P.Position) |
    Go |
    Quit |
    Unknown String deriving (Eq, Show)

data Response = Response [String] deriving (Eq, Show)
data Turk = Turk Handle

type CommandReader = IO Command
type ResponseWriter = Response -> IO ()


class Engine a where 
    getResponseFor :: a -> Command -> IO Response


instance Engine Turk where
    (Turk handle) `getResponseFor` Uci.Go = do
        move <- hGetLine handle
        return $ bestMoveResponse move

    _ `getResponseFor` Uci.Uci = return uciResponse

    _ `getResponseFor` Uci.IsReady = return readyOkResponse

    _ `getResponseFor` Uci.UciNewGame = return emptyResponse

    _ `getResponseFor` (Uci.Position _) = return emptyResponse

    _ `getResponseFor` (Uci.Unknown s) = return $ unknownResponse s

    _ `getResponseFor` Uci.Quit = return emptyResponse


data Currychnoi = Currychnoi (IORef (Maybe P.Position))

instance Engine Currychnoi where
    (Currychnoi ref) `getResponseFor` Uci.Go = do
        (Just position) <- readIORef ref
        move <- chooseMoveFrom position
        return $ bestMoveResponse $ show move

    (Currychnoi ref) `getResponseFor` (Uci.Position (Right position)) = do
        modifyIORef ref (\_ -> Just position)
        return emptyResponse

    _ `getResponseFor` Uci.Uci = return uciResponse

    _ `getResponseFor` Uci.IsReady = return readyOkResponse

    _ `getResponseFor` Uci.UciNewGame = return emptyResponse

    _ `getResponseFor` (Uci.Position (Left errorDesc)) = return $ unknownResponse errorDesc

    _ `getResponseFor` (Uci.Unknown s) = return $ unknownResponse s

    _ `getResponseFor` Uci.Quit = return emptyResponse


chooseMoveFrom :: P.Position -> IO Move
chooseMoveFrom position = do
    return $ findBestMove position


chooseRandomItem :: [a] -> IO a
chooseRandomItem xs = do
    i <- randomRIO (0, length xs - 1)
    return (xs !! i)


play :: Engine a => CommandReader -> a -> ResponseWriter -> IO ()
play reader engine writer = do
    command <- reader
    case command of
        Quit -> return ()
        _ -> do
            response <- engine `getResponseFor` command
            writer response
            play reader engine writer


stdinReader :: FilePath -> CommandReader
stdinReader logPath = do
    line <- getLine
    logLine <- makeLogLine "command" line
    appendFile logPath logLine
    return $ parse line


stdoutWriter :: FilePath -> Response -> IO ()
stdoutWriter logPath (Response responseLines) = do
    logLines <- mapM (makeLogLine "response") responseLines
    mapM_ (appendFile logPath) logLines
    putStr $ unlines responseLines
    hFlush stdout


makeLogLine :: String -> String -> IO String
makeLogLine kind line = do
    currentTime <- getCurrentTime
    let entries = [(show currentTime),  kind, line] in
        return $ (intercalate "\t" entries) ++ "\n"


uciResponse :: Response
uciResponse = Response ["id name chess", "id author Alexander Ershov", "uciok"]

readyOkResponse :: Response
readyOkResponse = Response ["readyok"]

emptyResponse :: Response
emptyResponse = Response []

bestMoveResponse :: String -> Response
bestMoveResponse move = Response ["bestmove " ++ move]

unknownResponse :: String -> Response
unknownResponse s = Response ["unknown command " ++ s]


parse :: String -> Command
parse "uci" = Uci
parse "isready" = IsReady
parse "ucinewgame" = UciNewGame
parse "quit" = Quit
parse s =
    case words s of
        "position":args -> parsePosition args
        "go":_ -> Go
        _ -> Unknown s


parsePosition :: [String] -> Command
parsePosition ["startpos"] = Position $ Right initialPosition
parsePosition ("startpos":"moves":moves) =
    Position $ makeMoves (Right initialPosition) (map parseMove movesWithSideToMove)
    where movesWithSideToMove = zip moves (cycle [White, Black])

parsePosition p = 
    Position $ Left $ "should be in the form `startpos moves ...`, got " ++ show p


type ParsedPosition = Either String P.Position

makeMoves :: ParsedPosition -> [Either String Move] -> ParsedPosition
makeMoves parsedPosition [] = parsedPosition
makeMoves parsedPosition (parsedMove:parsedMoves) = do
    position <- parsedPosition
    move <- parsedMove
    makeMoves (position `make` move) parsedMoves


parseMove :: (String, Color) -> Either String Move
parseMove (fromFile:fromRank:toFile:toRank:promotion', sideToMove) = do
    from <- parseSquare fromFile fromRank
    to <- parseSquare toFile toRank
    promotion <- parsePromotion promotion' sideToMove
    return $ Move from to promotion
parseMove (s, _) = Left $ "move should be in the form `f1f3`, got " ++ s


parsePromotion :: String -> Color -> Either String (Maybe Piece)
parsePromotion p color = 
    case p of 
        "" -> Right Nothing
        "n" -> Right $ Just $ Knight color
        "b" -> Right $ Just $ Bishop color
        "r" -> Right $ Just $ Rook color
        "q" -> Right $ Just $ Queen color
        _ -> Left $ "promotion should be one of `nbrq`, got " ++ p
