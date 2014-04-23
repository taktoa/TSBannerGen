module Utility ((//), (<$>), date) where
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import Data.Functor ((<$>))

x // y = fromIntegral x / fromIntegral y

date :: String -> IO String
date s = do
    time <- getClockTime >>= toCalendarTime
    return $ formatCalendarTime defaultTimeLocale s time
