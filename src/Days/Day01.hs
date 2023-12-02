module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isDigit)
{- ORMOLU_ENABLE -}

{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (takeTill isEndOfLine) `sepBy` endOfLine

------------ TYPES ------------
type Input = [Text]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
readNum :: Text -> Int
readNum input = read [Text.head numbers, Text.last numbers]
    where
        numbers = Text.filter isDigit input

partA :: Input -> OutputA
partA input = sum (List.map readNum input)

------------ PART B ------------
readNumB :: Text -> Int
readNumB input = read [Text.head numbers, Text.last numbers]
    where
        numbers = Text.filter isDigit (Text.pack (wordsToNumber (Text.unpack input)))

numberWords = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), 
               ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

wordsToNumber :: String -> String
wordsToNumber [] = []
wordsToNumber input = case List.find (\(w, _) -> w `List.isPrefixOf` input) numberWords of
    Just (w, n) -> n : wordsToNumber (List.tail input)
    Nothing -> (List.head input) : wordsToNumber (List.tail input)

partB :: Input -> OutputB
partB input =  sum (List.map readNumB input)

