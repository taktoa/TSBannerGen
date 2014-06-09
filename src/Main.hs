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
import Sinks.PNG
import Sinks.JSON
import Control.Monad.IO.Class (liftIO)
import Utility

tmp = "template.tmp"
fnt = "serif"
stl = "normal"
img = "base.png"
tft = "%H:%M:%S %Z | %B %e, %Y"


ss = SVGSettings tmp fnt stl img tft
ps = PNGSettings tmp fnt stl img tft

oldmain :: IO ()
oldmain = do
    mpd <- MPD.request
    mocp <- MOCP.request
    r <- genJSON [mocp]
    print r


renderSVG :: [String] -> Text -> IO ByteString
renderSVG o t = readProcessLBS rp ro svg
    where
    rp = "convert"
    ro = concat [["svg:-"], o, ["png:-"]]
    svg = toLazyByteString $ stringUtf8 $ T.unpack t

main = scotty 3000 $ do
--    r <- liftIO (genSVG ss [mpd])
--    s <- liftIO $ renderSVG ["-colors", "16"] (T.pack r)
    get "/banner.json" $ do
        setHeader "Content-Type" "application/json"
        mpd <- liftIO MPD.request
        j <- liftIO (genJSON [mpd])
        text $ TL.fromStrict j
    get "/banner.svg" $ do
        setHeader "Content-Type" "image/svg+xml"
        mpd <- liftIO MPD.request
        s <- liftIO (genSVG ss [mpd])
        text $ TL.fromStrict s
    get "/banner.png" $ do
        setHeader "Content-Type" "image/png"
        mpd <- liftIO MPD.request
        p <- liftIO (genPNG ps [mpd])
        text $ TL.fromStrict p

{-    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    r <- genSVG ss [mpd, mocp]
-}
