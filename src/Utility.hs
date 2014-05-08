module Utility ((//), (<$>), (<*>), date, Replacer, Filter, filterReplace, readProcessLBS, assert) where
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import Data.Functor ((<$>))
import Data.List (mapAccumL)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), (\\), keys)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<*>))


import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import System.Process
import System.Exit (ExitCode)
import System.IO
import Control.Concurrent

forkWait :: IO a -> IO (IO a)
forkWait a = do
    res <- newEmptyMVar
    _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
    return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)


readProcessWithExitCodeBS :: FilePath -> [String] -> SB.ByteString -> IO (ExitCode, SB.ByteString, SB.ByteString)
readProcessWithExitCodeBS cmd args input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args) { std_in  = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    flip onException
        (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        waitOut <- forkWait $ SB.hGetContents outh
        waitErr <- forkWait $ SB.hGetContents errh
        unless (SB.null input) $ do SB.hPutStr inh input; hFlush inh
        hClose inh
        out <- waitOut
        err <- waitErr
        hClose outh
        hClose errh
        ex <- waitForProcess pid
        return (ex, out, err)

readProcessBS :: FilePath -> [String] -> SB.ByteString -> IO SB.ByteString
readProcessBS c a i = snd3 <$> readProcessWithExitCodeBS c a i

--readProcessWithExitCodeLBS :: FilePath -> [String] -> LB.ByteString -> IO (ExitCode, LB.ByteString, LB.ByteString)
--readProcessWithExitCodeLBS = (liftM \(x,o,e) -> LB.fromStrict) . readProcessWithExitCodeBS

readProcessLBS :: FilePath -> [String] -> LB.ByteString -> IO LB.ByteString
readProcessLBS c a i = LB.fromStrict <$> readProcessBS c a (LB.toStrict i)

x // y = fromIntegral x / fromIntegral y

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

date :: String -> IO String
date s = do
    time <- getClockTime >>= toCalendarTime
    return $ formatCalendarTime defaultTimeLocale s time

type Replacer = Map String String
type Filter = Map String (String -> String)

applyFilters :: Filter -> Replacer -> Replacer
applyFilters f r = Map.mapWithKey (f'' !) r
    where
    f' = map ((flip Map.insert) (id)) (keys (r \\ f))
    f'' = ((flip $ foldr ($)) f') f

replaceCore :: Replacer -> [String] -> String
replaceCore r = concat . fromMaybe (error "Invalid parse") . g
    where
    g = sequence . f
    f = (\(x, y) -> if odd x then y else Nothing:y) . mapAccumL (\a x -> (a + 1, if odd a then Map.lookup x r else return x)) 0

replace :: String -> Replacer -> String -> String
replace del r s = replaceCore r (splitOn del s)

filterReplace :: String -> Filter -> [Replacer] -> (String -> String)
filterReplace del f rs = replace del $ applyFilters f (Map.unions rs)
