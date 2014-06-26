module Sinks.PNG (genPNG, PNGSettings (..)) where
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Builder (toLazyByteString, stringUtf8)
import Data.Text (Text, unpack)
import Sinks.SVG
import Utility

data PNGSettings = PNGSettings {
        options  :: [String],
        template :: String,
        font     :: String,
        style    :: String,
        image    :: String,
        tsfmt    :: String
    } deriving (Show, Read)

pstoss (PNGSettings _ t f s i tf) = SVGSettings t f s i tf

renderSVG :: [String] -> Text -> IO ByteString
renderSVG o t = readProcessLBS rp ro svg
    where
    rp = "convert"
    ro = concat [["svg:-"], o, ["png:-"]]
    svg = toLazyByteString $ stringUtf8 $ unpack t

genPNG :: PNGSettings -> [Replacer] -> IO ByteString
genPNG ps rs = genSVG (pstoss ps) rs >>= (\svg -> renderSVG (options ps) svg)
