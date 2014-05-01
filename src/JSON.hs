module JSON (genJSON) where
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), (\\), keys, Map, toList, unions)
import Utility

renderTuple :: (String, String) -> String
renderTuple (k, v) = show k ++ ": " ++ show v

renderList :: [String] -> String
renderList xs = "{\n" ++ (concatMap (\a -> "    " ++ a ++ ",\n") xs) ++ "}"

renderTuples :: [(String, String)] -> String
renderTuples = renderList . map renderTuple

renderMap :: Map String String -> String
renderMap = renderTuples . toList

genJSON :: [Replacer] -> IO String
genJSON = return . renderMap . unions
