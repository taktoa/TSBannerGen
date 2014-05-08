module Main (main) where
--import Network.CGI (setHeader, outputFPS, runCGI, handleErrors)
import Web.Scotty
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Builder (toLazyByteString, stringUtf8)
import Data.Monoid (mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Sources.MPD as MPD
import qualified Sources.MOCP as MOCP
import Sinks.SVG
import Sinks.JSON
import Control.Monad.IO.Class (liftIO)
import Utility

tmp = "template.tmp"
fnt = "serif"
stl = "normal"
img = "base.png"
tft = "%H:%M:%S %Z | %B %e, %Y"


oldmain :: IO ()
oldmain = do
    mpd  <- MPD.request
    mocp  <- MOCP.request
    let ss = SVGSettings tmp fnt stl img tft
    r <- genJSON [mocp]
    print r


renderSVG :: [String] -> Text -> IO ByteString
renderSVG o t = readProcessLBS rp ro svg
    where
    rp = "convert"
    ro = concat [["svg:-"], o, ["png:-"]]
    svg = toLazyByteString $ stringUtf8 $ T.unpack t

main = scotty 3000 $ do
    mpd <- liftIO MPD.request
    j <- liftIO (genJSON [mpd])
    get "/json" $ do
        text $ TL.fromStrict j

{-    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    r <- genSVG ss [mpd, mocp]
-}
