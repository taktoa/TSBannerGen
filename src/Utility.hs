module Utility ((//), (<$>), date, Replacer, Filter, filterReplace) where
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import Data.Functor ((<$>))
import Data.List (mapAccumL)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), (\\), keys)
import Data.Maybe (fromMaybe)

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
