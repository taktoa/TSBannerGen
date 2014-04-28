module Main (main) where
import Network.CGI
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.Builder (toLazyByteString, stringUtf8)
import qualified MPD
import qualified MOCP
import GenSVG
import Utility

tmp = "test.tmp"
fnt = "serif"
stl = "normal"
img = "base.png"
tft = "%H:%M:%S %Z | %B %e, %Y"


main :: IO ()
main = do
    mpd  <- MPD.request
    mocp <- MOCP.request
    let ss = SVGSettings tmp fnt stl img tft
    r <- genSVG ss [mpd, mocp]
    let svg = toStrict $ toLazyByteString $ stringUtf8 r
    png <- readProcessBS "convert" ["svg:-", "-colors", "16", "png:-"] svg
    runCGI $ handleErrors $ do
        setHeader "Content-type" "image/png"
        outputFPS $ fromStrict png
