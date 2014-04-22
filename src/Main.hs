module Main (main) where
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import Network.CGI
import Data.String.Utils (replace, split)
import Data.Either
import MPD
import MOCP

tmp = "template.tmp"
font = "serif"
style = "normal"
image = "base.png"
tsfmt = "%H:%M:%S %Z | %B %e, %Y"

x // y = fromIntegral x / fromIntegral y

date :: String -> IO String
date s = do
    time <- getClockTime >>= toCalendarTime
    return $ formatCalendarTime defaultTimeLocale s time

main :: IO ()
main = do
    ss <- getSongStatus
    let sts = either (\x -> error (show x)) (id) ss
    let (SongStatus fl ti ar al st pe) = sts
    let file = if fl == "" then "" else take 54 $ last $ split "/" fl
    let (title, artist, album) = (take 54 ti, take 54 ar, take 54 al)
    let status = case st of
            Playing -> "&#9654;"
            Paused -> "&#10073;&#10073;"
            Stopped -> "&#9726;"
    let percent = show $ round $ (*100) $ uncurry (//) $ pe
    timestamp <- date tsfmt
    let runReplace =  [ replace "SONG_FILE" file,
                        replace "SONG_TITLE" title,
                        replace "SONG_ARTIST" artist,
                        replace "SONG_ALBUM" album,
                        replace "SONG_STATUS" status,
                        replace "SONG_PERCENT" percent,
                        replace "INFO_FONT" font,
                        replace "INFO_STYLE" style,
                        replace "BANNER_IMAGE" image,
                        replace "TIMESTAMP" timestamp ]
    let replacer = (flip $ foldr ($)) runReplace
    template <- readFile tmp
    putStrLn $ replacer template
    --runCGI $ handleErrors (output $ show st)
