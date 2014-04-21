--module MPD (SongStatus, sFile, sTitle, sArtist, sAlbum, sState, sPercent, getSongStatus) where
module MPD (SongStatus (..), getSongStatus, State (..)) where
import Network.MPD
import Data.Either
import Data.Maybe

-- File, Title, Artist, Album, State, Percent

data SongStatus = SongStatus {
        sFile    :: String,
        sTitle   :: String,
        sArtist  :: String,
        sAlbum   :: String,
        sState   :: State,
        sPercent :: Int
    } deriving (Show)

type MPDState = (Song, Status)

sngFile :: MPDState -> String
sngFile = toString . sgFilePath . fst

tagHelper :: Metadata -> MPDState -> String
tagHelper m s = fromMaybe "" $ (toString . head) `fmap` (sgGetTag m (fst s))

sngTitle :: MPDState -> String
sngTitle = tagHelper Title

sngArtist :: MPDState -> String
sngArtist = tagHelper Artist

sngAlbum :: MPDState -> String
sngAlbum = tagHelper Album

sngState :: MPDState -> State
sngState = stState . snd

sngPercent :: MPDState -> Int
sngPercent = round . tupleToPercent . stTime . snd
    where
    tupleToPercent (x, y) = 100 * x / fromIntegral y

songStatus :: MPD SongStatus
songStatus = do
    sng <- currentSong
    sts <- status
    if isJust sng then do
        let ms = (fromMaybe undefined sng, sts)
        let fl = sngFile ms
        let ti = sngTitle ms
        let ar = sngArtist ms
        let al = sngAlbum ms
        let st = sngState ms
        let pe = sngPercent ms
        return (SongStatus fl ti ar al st pe)
    else
        return (SongStatus "" "" "" "" Stopped 0)

getSongStatus :: IO (Response SongStatus)
getSongStatus = withMPD $ songStatus
