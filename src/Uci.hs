{-# LANGUAGE BlockArguments #-}

module Uci where

import Data.IORef
import Data.List
import Data.Time (getCurrentTime)
import System.IO
import System.Random

import qualified Position as P
import Position hiding (Position)
import Squares

data Command = 
    Uci |
    IsReady |
    UciNewGame |
    Position (Either ErrorDesc P.Position) |
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
    chooseRandomItem $ legalMoves position


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
    Position $ makeMoves (Right initialPosition) (map parseMove moves)

parsePosition p = 
    Position $ Left $ "should be in the form `startpos moves ...`, got " ++ show p


type ParsedPosition = Either ErrorDesc P.Position

makeMoves :: ParsedPosition -> [Either ErrorDesc P.Move] -> ParsedPosition
makeMoves parsedPosition [] = parsedPosition
makeMoves parsedPosition (parsedMove:parsedMoves) = do
    position <- parsedPosition
    move <- parsedMove
    makeMoves (position `make` move) parsedMoves


parseMove :: String -> Either ErrorDesc Move
parseMove [fromFile, fromRank, toFile, toRank] = do
    from <- parseSquare fromFile fromRank
    to <- parseSquare toFile toRank
    return $ Move from to
parseMove s = Left $ "move should be in the form `f1f3`, got " ++ s


parseSquare :: Char -> Char -> Either ErrorDesc Square
parseSquare f r = do
    file <- parseFile f
    rank <- parseRank r
    return (file, rank)


parseFile :: Char -> Either ErrorDesc Int
parseFile f =
    case f of
        'a' -> Right 1
        'b' -> Right 2
        'c' -> Right 3
        'd' -> Right 4
        'e' -> Right 5
        'f' -> Right 6
        'g' -> Right 7
        'h' -> Right 8
        _ -> Left $ "file should be one of `abcdefgh`, got " ++ [f]


parseRank :: Char -> Either ErrorDesc Int
parseRank r =
    case r of
        '1' -> Right 1
        '2' -> Right 2
        '3' -> Right 3
        '4' -> Right 4
        '5' -> Right 5
        '6' -> Right 6
        '7' -> Right 7
        '8' -> Right 8
        _ -> Left $ "rank should be one of`12345678`, got " ++ [r]
