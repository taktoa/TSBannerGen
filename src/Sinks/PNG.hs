module PNG where
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Builder (toLazyByteString, stringUtf8)
import Data.Monoid (mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Sinks.SVG
import Control.Monad.IO.Class (liftIO)
import Utility

data SVGSettings = SVGSettings {
        template :: String,
        font     :: String,
        style    :: String,
        image    :: String,
        tsfmt    :: String
    } deriving (Show, Read)

renderSVG :: [String] -> Text -> IO ByteString
renderSVG o t = readProcessLBS rp ro svg
    where
    rp = "convert"
    ro = concat [["svg:-"], o, ["png:-"]]
    svg = toLazyByteString $ stringUtf8 $ T.unpack t

genPNG :: PNGSettings -> [Replacer] -> IO Text

