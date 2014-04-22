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
        sTime    :: (Int, Int)
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

sngTime :: MPDState -> (Int, Int)
sngTime = (\(x, y) -> (round x, fromIntegral y)) . stTime . snd

songStatus' :: MPD (Maybe SongStatus)
songStatus' = do
    sng <- currentSong
    sts <- status
    return $ do
        msng <- sng
        let ms = (msng, sts)
        let fl = sngFile ms
        let ti = sngTitle ms
        let ar = sngArtist ms
        let al = sngAlbum ms
        let st = sngState ms
        let tm = sngTime ms
        return (SongStatus fl ti ar al st tm)

songStatus :: MPD SongStatus
songStatus = do
    ss <- songStatus'
    let stop = (SongStatus "" "" "" "" Stopped (0, 1))
    return $ fromMaybe stop ss

getSongStatus :: IO (Response SongStatus)
getSongStatus = withMPD $ songStatus
