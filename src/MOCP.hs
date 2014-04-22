module MOCP where
import System.Process
import State

mocpQuery :: String -> IO String
mocpQuery s = readProcess "mocp" ["-q", s] ""

stateQuery :: IO State
stateQuery = convert `fmap` mocpQuery "%state"
    where
    convert "PLAY" = Playing
    convert "PAUSE" = Paused
    convert "STOP" = Stopped
    convert _ = error "Unknown response"

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
    return (cs, ts)


