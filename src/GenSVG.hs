module GenSVG (SVGSettings (..), genSVG) where
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), (\\), keys)
import Data.Maybe (fromMaybe)
import Data.Either
import Utility

data SVGSettings = SVGSettings {
        template :: String,
        font     :: String,
        style    :: String,
        image    :: String,
        tsfmt    :: String
    } deriving (Show, Read)

genSVG :: SVGSettings -> [Replacer] -> IO String
genSVG s rs = do
    let (SVGSettings tmp ft stl img tf) = s
    let cutLen = take 54
    let fileTail = last . splitOn "/"
    let stateProc st = case st of
            "PLAY" ->  "&#9654;"
            "PAUSE" -> "&#10073;&#10073;"
            "STOP" ->  "&#9726;"
            _ ->       "?"
    let timeProc = show . round . (*100) . uncurry (//) . (\x -> read x :: (Int, Int))
    let filt = Map.fromList [("mpd-file",    cutLen . fileTail),
                             ("mocp-file",   cutLen . fileTail),
                             ("mpd-title",   cutLen),
                             ("mocp-title",  cutLen),
                             ("mpd-artist",  cutLen),
                             ("mocp-artist", cutLen),
                             ("mpd-album",   cutLen),
                             ("mocp-album",  cutLen),
                             ("mpd-state",   stateProc),
                             ("mocp-state",  stateProc),
                             ("mpd-time",    timeProc),
                             ("mocp-time",   timeProc)]
    timestamp <- date tf
    let svgs = Map.fromList [("svg-font", ft),
                             ("svg-style", stl),
                             ("svg-image", img),
                             ("svg-time", timestamp)]
    template <- readFile tmp
    return $ filterReplace "$" filt (svgs:rs) template
