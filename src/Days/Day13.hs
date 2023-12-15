module Days.Day13 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parsePattern `sepBy1'` (endOfLine *> endOfLine)
    where 
        parseLine = many1' ((char '.' <|> char '#'))
        parsePattern = parseLine `sepBy1'` endOfLine


------------ TYPES ------------
type Input = [Pattern]
type Pattern = [String]

type OutputA = Void

type OutputB = Void

------------ PART A ------------
potentialMirror :: Pattern -> [Int]
potentialMirror pattern = map (\(_,_,i) -> i) (filter (\(a,b,i) -> a == b)  (zip3 pattern (tail pattern) [0..]))

isMirror :: Pattern -> Int -> Maybe Int
isMirror pattern idx = if mirrorLength > 0 && (take mirrorLength (reverse left)) == (take mirrorLength right)
                            then Just idx
                            else Nothing
    where
        (left, right) = splitAt idx pattern
        mirrorLength = min (length left) (length right)

mirrorRows :: Pattern -> [Int] 
mirrorRows pattern = mapMaybe (isMirror pattern) [0..(length pattern)]

summarize :: Pattern -> Int
summarize pattern = colValue + rowValue
    where
        rowValue = 100 * sum (mirrorRows pattern)
        colValue = sum (mirrorRows (transpose pattern))

-- partA :: Input -> OutputA
partA input = sum (map summarize input)

    -- filter (isMirror (last input)) (potentialMirror (last input))

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
