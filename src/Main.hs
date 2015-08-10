module Main (main) where

import           Control.Monad.IO.Class       (liftIO)
import           Data.ByteString.Lazy         (ByteString)
import           Data.ByteString.Lazy.Builder (stringUtf8, toLazyByteString)
import           Data.Monoid                  (mconcat)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Sinks.JSON
import           Sinks.PNG
import           Sinks.SVG
import qualified Sources.MOCP                 as MOCP
import qualified Sources.MPD                  as MPD
import           Utility
import           Web.Scotty

tmp = "template.tmp"
fnt = "serif"
stl = "normal"
img = "base.png"
tft = "%H:%M:%S %Z | %B %e, %Y"
opt = ["-colors", "16"]

ss = SVGSettings tmp fnt stl img tft
ps = PNGSettings opt tmp fnt stl img tft

renderSVG :: [String] -> Text -> IO ByteString
renderSVG o t = readProcessLBS rp ro svg
    where
    rp = "convert"
    ro = concat [["svg:-"], o, ["png:-"]]
    svg = toLazyByteString $ stringUtf8 $ T.unpack t

main = scotty 3000 $ do
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
        raw p
