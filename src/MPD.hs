module MPD (getMusicStatus) where
import Network.MPD
import Data.Either
import Data.Maybe
import MusicStatus
import Utility

type MPDState = (Song, Status)

sngFile :: MPDState -> String
sngFile = toString . sgFilePath . fst

tagHelper :: Metadata -> MPDState -> String
tagHelper m s = fromMaybe "" $ (toString . head) <$> (sgGetTag m (fst s))

sngTitle :: MPDState -> String
sngTitle = tagHelper Title

sngArtist :: MPDState -> String
sngArtist = tagHelper Artist

sngAlbum :: MPDState -> String
sngAlbum = tagHelper Album

sngState :: MPDState -> MusicState
sngState = conv . stState . snd
    where
    conv Playing = PLAY
    conv Paused  = PAUSE
    conv Stopped = STOP

sngTime :: MPDState -> (Int, Int)
sngTime = (\(x, y) -> (round x, fromIntegral y)) . stTime . snd

musicStatus :: MPD MusicStatus
musicStatus = do
    sng <- currentSong
    sts <- status
    let stop = (MusicStatus "" "" "" "" STOP (0, 1))
    return $ fromMaybe stop $ do
        msng <- sng
        let ms = (msng, sts)
        let fl = sngFile ms
        let ti = sngTitle ms
        let ar = sngArtist ms
        let al = sngAlbum ms
        let st = sngState ms
        let tm = sngTime ms
        return (MusicStatus fl ti ar al st tm)

getMusicStatus :: IO MusicStatus
getMusicStatus = either (error . show) (id) <$> (withMPD $ musicStatus)
        
