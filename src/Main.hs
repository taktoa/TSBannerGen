module Main (main) where
import Network.CGI
import MusicStatus
import Data.String.Utils (replace, split)
import Data.Either
import MPD as MPD
import MOCP as MOCP
import Utility (date, (//))

tmp = "template.tmp"
font = "serif"
style = "normal"
image = "base.png"
tsfmt = "%H:%M:%S %Z | %B %e, %Y"

main :: IO ()
main = do
    ms <- MPD.getMusicStatus
    let (MusicStatus fl ti ar al st pe) = ms
    let file = if fl == "" then "" else take 54 $ last $ split "/" fl
    let (title, artist, album) = (take 54 ti, take 54 ar, take 54 al)
    let status = case st of
            PLAY ->  "&#9654;"
            PAUSE -> "&#10073;&#10073;"
            STOP ->  "&#9726;"
    let percent = show $ round $ (*100) $ uncurry (//) $ pe
    timestamp <- date tsfmt
    let runReplace = map (uncurry replace)
                      [ ("SONG_FILE", file),
                        ("SONG_TITLE", title),
                        ("SONG_ARTIST", artist),
                        ("SONG_ALBUM", album),
                        ("SONG_STATUS", status),
                        ("SONG_PERCENT", percent),
                        ("INFO_FONT", font),
                        ("INFO_STYLE", style),
                        ("BANNER_IMAGE", image),
                        ("TIMESTAMP", timestamp) ]
    let replacer = (flip $ foldr ($)) runReplace
    template <- readFile tmp
    putStrLn $ replacer template
    --runCGI $ handleErrors (output $ show st)
