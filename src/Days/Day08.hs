module Days.Day08 (runDay) where

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
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do 
    instructions <- many1' letter
    endOfLine
    endOfLine
    nodes <- parseNodes
    return (instructions, Map.fromList nodes)
    where
        parseNodes = parseNode `sepBy1'` endOfLine

parseNode = do
    start <- many1' (notChar ' ')
    string " = ("
    l <- many1' (notChar ',')
    string ", "
    r <- many1' (notChar ')')
    char ')'
    return (start, (l, r))


------------ TYPES ------------
type Input = (String, Map String (String, String))

type OutputA = Int

type OutputB = Int

------------ PART A ------------

doStep :: Map String (String, String) -> String -> Char -> String
doStep nodes current 'L' = fst (nodes Map.! current)
doStep nodes current 'R' = snd (nodes Map.! current)

partA :: Input -> OutputA
partA (inst, nodes) = length (takeWhile (/= "ZZZ") steps)
    where
        fullInstructions = cycle inst
        steps = scanl (doStep nodes) "AAA" fullInstructions 

------------ PART B ------------
endsWith :: Char -> String -> Bool
endsWith c s= last s == c

findCycleLength :: [String] -> Int
findCycleLength path = snd (head (filter (\(s, _) -> endsWith 'Z' s) (zip path [0..])))

partB :: Input -> OutputB
partB (inst, nodes) = foldl1 lcm cycleLengths
    where
        fullInstructions = cycle inst
        starts = filter (endsWith 'A') (Map.keys nodes)
        cycleLengths = map (findCycleLength . (\s -> scanl (doStep nodes) s fullInstructions)) starts
