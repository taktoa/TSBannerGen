module MPD (request) where
import Network.MPD
import Data.Map.Strict as Map
import Data.Either
import Data.Maybe
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

sngState :: MPDState -> String
sngState = conv . stState . snd
    where
    conv Playing = "PLAY"
    conv Paused  = "PAUSE"
    conv Stopped = "STOP"

sngTime :: MPDState -> String
sngTime = show . (\(x, y) -> (round x, fromIntegral y)) . stTime . snd

musicStatus :: MPD Replacer
musicStatus = do
    sng <- currentSong
    sts <- status
    let stop = [("mpd-file", ""),
                ("mpd-title", ""),
                ("mpd-artist", ""),
                ("mpd-album", ""),
                ("mpd-state", "STOP"),
                ("mpd-time", "(0, 1)")]
    return $ fromMaybe (Map.fromList stop) $ do
        msng <- sng
        let ms = (msng, sts)
        let fi = sngFile ms
        let ti = sngTitle ms
        let ar = sngArtist ms
        let al = sngAlbum ms
        let st = sngState ms
        let tm = sngTime ms
        let kv = [("mpd-file", fi),
                  ("mpd-title", ti),
                  ("mpd-artist", ar),
                  ("mpd-album", al),
                  ("mpd-state", st),
                  ("mpd-time", tm)]
        return $ Map.fromList kv

request :: IO Replacer
request = either (error . show) (id) <$> (withMPD $ musicStatus)

