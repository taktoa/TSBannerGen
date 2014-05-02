module Sources.MPD (request) where
import Network.MPD as MPD
import Network.MPD (MPD)
import Data.Map.Strict as Map
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Utility

type MPDState = (MPD.Song, MPD.Status)

sngFile :: MPDState -> String
sngFile = MPD.toString . MPD.sgFilePath . fst

tagHelper :: Metadata -> MPDState -> String
tagHelper m s = fromMaybe "" $ (MPD.toString . head) <$> (MPD.sgGetTag m (fst s))

sngTitle :: MPDState -> String
sngTitle = tagHelper Title

sngArtist :: MPDState -> String
sngArtist = tagHelper Artist

sngAlbum :: MPDState -> String
sngAlbum = tagHelper Album

sngState :: MPDState -> String
sngState = conv . MPD.stState . snd
    where
    conv Playing = "PLAY"
    conv Paused  = "PAUSE"
    conv Stopped = "STOP"

sngTime :: MPDState -> String
sngTime = show . (\(x, y) -> (round x, fromIntegral y)) . MPD.stTime . snd

types = ["mpd-file", "mpd-title", "mpd-artist", "mpd-album", "mpd-state", "mpd-time"]

createReplacer :: [String] -> Replacer
createReplacer = fromList . zip types . check
    where
    check xs = assert (length xs == length types) xs

defaultState :: Replacer
defaultState = createReplacer ["", "", "", "", "STOP", "(0, 1)"]

packMusic :: MPDState -> Replacer
packMusic ms = createReplacer [fi, ti, ar, al, st, tm]
    where
    fi = sngFile ms
    ti = sngTitle ms
    ar = sngArtist ms
    al = sngAlbum ms
    st = sngState ms
    tm = sngTime ms

musicStatus :: MPD Replacer
musicStatus = do
    sng <- MPD.currentSong
    sts <- MPD.status
    let r = packMusic <$> (,sts) <$> sng
    return $ fromMaybe defaultState r

request :: IO Replacer
request = either (error . show) (id) <$> (withMPD $ musicStatus)

