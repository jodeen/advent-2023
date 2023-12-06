module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
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
import Data.Attoparsec.ByteString.Char8 (isSpace)
{- ORMOLU_ENABLE -}
{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    string "Time:"
    skipSpace
    time <- parseNumbers
    endOfLine
    string "Distance:"
    skipSpace
    zip time <$> parseNumbers
    where
        parseNumbers = decimal `sepBy1'` (Data.Attoparsec.Text.takeWhile isSpace)

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
calcDistance :: Int -> Int -> Int
calcDistance raceDuration holdTime = holdTime * (raceDuration - holdTime)

countWins :: (Int, Int) -> Int
countWins (time, distance) = length (filter (> distance) (map (calcDistance time) [1..time]))

partA :: Input -> OutputA
partA input = product (map countWins input)

------------ PART B ------------
partB :: Input -> OutputB
partB input = countWins (time, distance)
    where
        (times, distances) = unzip input
        time = read (concatMap show times)
        distance = read (concatMap show distances)
