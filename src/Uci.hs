{-# LANGUAGE BlockArguments #-}

module Uci where

import Data.List
import Data.Time (getCurrentTime)
import System.IO

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
type Player = IO String

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


getResponse :: Command -> Player -> IO Response

getResponse Uci.Uci _ = return uciResponse

getResponse Uci.IsReady _ = return readyOkResponse

getResponse Uci.UciNewGame _ = return emptyResponse

getResponse (Uci.Position _) _ = return emptyResponse

getResponse Uci.Go player = do
    move <- player
    return $ bestMoveResponse move

getResponse (Uci.Unknown s) _ = return $ unknownResponse s

getResponse Uci.Quit _ = return emptyResponse


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
parsePosition ("startpos":"moves":moves) =
    Position $ makeMoves (Right initialPosition) (map parseMove moves)
parsePosition p = Position $ Left $ "should be in the form `startpos moves ...`, got " ++ show p


type ParsedPosition = Either ErrorDesc P.Position

makeMoves :: ParsedPosition -> [Either ErrorDesc P.Move] -> ParsedPosition
makeMoves parsedPosition [] = parsedPosition
makeMoves parsedPosition@(Left _) _ = parsedPosition
makeMoves _ (Left s:_) = Left s
makeMoves (Right position) (Right move:moves) = makeMoves (position `make` move) moves


parseMove :: String -> Either ErrorDesc Move
parseMove [fromFile, fromRank, toFile, toRank] = do
    from <- parseSquare fromFile fromRank
    to <- parseSquare toFile toRank
    return $ Move from to
parseMove s = Left $ "move should be in the form `f1f3`, got " ++ s


parseSquare :: Char -> Char -> Either ErrorDesc Square
parseSquare file' rank' = do
    file <- parseFile file'
    rank <- parseRank rank'
    return (file, rank)


parseFile :: Char -> Either ErrorDesc Int
parseFile 'a' = Right 1
parseFile 'b' = Right 2
parseFile 'c' = Right 3
parseFile 'd' = Right 4
parseFile 'e' = Right 5
parseFile 'f' = Right 6
parseFile 'g' = Right 7
parseFile 'h' = Right 8
parseFile file = Left $ "file should be `abcdefgh`, got " ++ [file]


parseRank :: Char -> Either ErrorDesc Int
parseRank '1' = Right 1
parseRank '2' = Right 2
parseRank '3' = Right 3
parseRank '4' = Right 4
parseRank '5' = Right 5
parseRank '6' = Right 6
parseRank '7' = Right 7
parseRank '8' = Right 8
parseRank rank = Left $ "rank should be `12345678`, got " ++ [rank]
