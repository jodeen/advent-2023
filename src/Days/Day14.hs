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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import GHC.IO (unsafePerformIO)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1' (notChar '\n')) `sepBy1'` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

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

substr :: String -> (Int, Int) -> String
substr string (start, end) = take (end-start) (drop start string)

rollLeft :: String -> String
rollLeft string = take (length string) (intercalate "#" (map (reverse . sort) substrings))
    where
        hashes = elemIndices '#' string
        segments = zipWith (\a b -> (a+1, b)) (-1:hashes) (hashes++[length string])
        substrings = map (substr string) segments

doTilt :: [String] -> Char -> [String]
doTilt input 'N' = transpose (doTilt (transpose input) 'W')
doTilt input 'W' = map rollLeft input
doTilt input 'S' = transpose (doTilt (transpose input) 'E')
doTilt input 'E' = map reverse (map rollLeft (map reverse input))

doCycle input = doTilt (doTilt (doTilt  (doTilt input 'N') 'W') 'S') 'E'

calcLoad :: [String] -> Int
calcLoad state = sum (zipWith (\items value -> length (filter (=='O') items) * value) state (reverse [1..(length state)]))

findCycle :: Int -> [[String]]  -> [String]
findCycle startIdx (start:rest)  = finalPoint
    where
        Just nextIndex = elemIndex start rest
        finalIdx = mod (1000000000 - startIdx) (nextIndex + 1)
        finalPoint = (start:rest) !! (finalIdx + 1)

partB :: Input -> OutputB
partB input =  calcLoad (findCycle 5000 (take 20 (drop 4999 cycles)))
    where
        cycles = iterate doCycle input


