module Sinks.JSON (genJSON) where
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), (\\), keys, Map, toList, unions)
import Data.Text (pack, Text)
import Utility

renderTuple :: (String, String) -> String
renderTuple (k, v) = show k ++ ": " ++ show v

renderList :: [String] -> String
renderList xs = "{\n" ++ (concatMap (\a -> "    " ++ a ++ ",\n") xs) ++ "}"

renderTuples :: [(String, String)] -> Text
renderTuples = pack . renderList . map renderTuple

renderMap :: Map String String -> Text
renderMap = renderTuples . toList

genJSON :: [Replacer] -> IO Text
genJSON = return . renderMap . unions
