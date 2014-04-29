module MOCP (request) where
import System.Process
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Utility

query :: String -> IO String
query s = init <$> readProcess "mocp" ["-Q", s] ""

mocpQuery :: IO [String]
mocpQuery = do
    let del = "|||"
    let qu = ["%file", "%song", "%artist", "%album", "%state", "%cs", "%ts"]
    q <- query $ intercalate del qu
    return $ splitOn del q

request :: IO Replacer
request = do
    q <- mocpQuery
    when (length q /= 7) (error "MOCP query failed")
    let [fi, ti, ar, al, st, cs, ts] = q
    let tm = show (read cs :: Int, read ts :: Int)
    let kv =    [("mocp-file", fi),
                 ("mocp-title", ti),
                 ("mocp-artist", ar),
                 ("mocp-album", al),
                 ("mocp-state", st),
                 ("mocp-time", tm)]
    return $ Map.fromList kv
