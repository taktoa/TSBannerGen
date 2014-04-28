module Utility ((//), (<$>), date, Replacer, Filter, filterReplace, readProcessBS) where
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import Data.Functor ((<$>))
import Data.List (mapAccumL)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), (\\), keys)
import Data.Maybe (fromMaybe)

import qualified Control.Exception as C
import Control.Monad
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Process
import System.Exit (ExitCode (..))
import System.IO

readProcessBS :: FilePath -> [String] -> ByteString -> IO ByteString
readProcessBS cmd args input = do
    (Just inh, Just outh, _, pid) <- createProcess (proc cmd args) { std_in  = CreatePipe, std_out = CreatePipe, std_err = Inherit }
    output  <- B.hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ C.evaluate (B.length output) >> putMVar outMVar ()
    when (not (B.null input)) $ do { B.hPutStr inh input; hFlush inh }
    hClose inh
    takeMVar outMVar
    hClose outh
    ex <- waitForProcess pid
    case ex of
        ExitSuccess   -> return output
        ExitFailure r -> error ("readProcess: " ++ cmd ++ ' ':unwords (map show args) ++ " (exit " ++ show r ++ ")")





x // y = fromIntegral x / fromIntegral y

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
