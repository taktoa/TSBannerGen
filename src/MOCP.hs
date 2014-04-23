module MOCP (getMusicStatus) where
import System.Process
import MusicStatus
import Utility

mocpQuery :: String -> IO String
mocpQuery s = init <$> readProcess "mocp" ["-Q", s] ""

stateQuery :: IO MusicState
stateQuery = read <$> mocpQuery "%state"

fileQuery :: IO String
fileQuery = mocpQuery "%file"

titleQuery :: IO String
titleQuery = mocpQuery "%song"

artistQuery :: IO String
artistQuery = mocpQuery "%artist"

albumQuery :: IO String
albumQuery = mocpQuery "%album"

timeQuery :: IO (Int, Int)
timeQuery = do
    cs <- mocpQuery "%cs"
    ts <- mocpQuery "%ts"
    return (read cs, read ts)

getMusicStatus :: IO MusicStatus
getMusicStatus = do
    fi <- fileQuery
    ti <- titleQuery
    ar <- artistQuery
    al <- albumQuery
    st <- stateQuery
    tm <- timeQuery
    return (MusicStatus fi ti ar al st tm)
