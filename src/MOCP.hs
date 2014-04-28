module MOCP (request) where
import System.Process
import qualified Data.Map.Strict as Map
import Utility

mocpQuery :: String -> IO String
mocpQuery s = init <$> readProcess "mocp" ["-Q", s] ""

stateQuery :: IO String
stateQuery = mocpQuery "%state"

fileQuery :: IO String
fileQuery = mocpQuery "%file"

titleQuery :: IO String
titleQuery = mocpQuery "%song"

artistQuery :: IO String
artistQuery = mocpQuery "%artist"

albumQuery :: IO String
albumQuery = mocpQuery "%album"

timeQuery :: IO String
timeQuery = do
    cs <- mocpQuery "%cs"
    ts <- mocpQuery "%ts"
    return $ show (read cs :: Int, read ts :: Int)

request :: IO Replacer
request = do
    fi <- fileQuery
    ti <- titleQuery
    ar <- artistQuery
    al <- albumQuery
    st <- stateQuery
    tm <- timeQuery
    let kv =    [("mocp-file", fi),
                 ("mocp-title", ti),
                 ("mocp-artist", ar),
                 ("mocp-album", al),
                 ("mocp-state", st),
                 ("mocp-time", tm)]
    return $ Map.fromList kv
