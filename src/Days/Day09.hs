module Days.Day09 (runDay) where

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
{- ORMOLU_ENABLE -}
{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ((signed decimal) `sepBy1'` (char ' ')) `sepBy1'` endOfLine

------------ TYPES ------------
type Input = [[Integer]]

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
pairwiseDifference :: [Integer] -> [Integer]
pairwiseDifference a = zipWith (-) (tail a) a


findNext ints 
        | all (==0) ints = 0
        | otherwise = (last ints) + (findNext diff)
    where 
        diff = pairwiseDifference ints

-- partA :: Input -> OutputA
partA input = sum (map findNext input)

------------ PART B ------------
findPrev ints 
        | all (==0) ints = 0
        | otherwise = (head ints) - (findPrev diff)
    where 
        diff = pairwiseDifference ints

-- partB :: Input -> OutputB
partB input = sum (map findPrev input)
