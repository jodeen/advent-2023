module Days.Day14 (runDay) where

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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1' (notChar '\n')) `sepBy1'` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Void

------------ PART A ------------
countLeft idx weightValue ('.':xs) = countLeft (idx-1) weightValue xs
countLeft idx weightValue ('O':xs) = weightValue + countLeft (idx-1) (weightValue-1) xs
countLeft idx weightValue ('#':xs) = countLeft (idx-1) (idx-1) xs
countLeft _ _ [] = 0

partA :: Input -> OutputA
partA input = sum (map (countLeft len len) swapped)
    where
        swapped = transpose input
        len = length (head swapped)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
