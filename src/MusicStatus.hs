module MusicStatus (MusicStatus (..), MusicState (..)) where
-- File, Title, Artist, Album, State, Percent

data MusicStatus = MusicStatus {
        mFile    :: String,
        mTitle   :: String,
        mArtist  :: String,
        mAlbum   :: String,
        mState   :: MusicState,
        mTime    :: (Int, Int)
    } deriving (Show, Read)

data MusicState = PLAY | PAUSE | STOP deriving (Show, Read)
