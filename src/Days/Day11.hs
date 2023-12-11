module Days.Day11 (runDay) where

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
import Data.Functor (($>))
import Options.Applicative ((<|>))
{- ORMOLU_ENABLE -}
{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1' ((char '.' $> False) <|>  (char '#' $> True))) `sepBy1'` endOfLine


------------ TYPES ------------
type Input = [[Bool]]
type Point = (Int, Int)

type OutputA = Int

type OutputB = Integer

------------ PART A ------------
doGrow :: Input -> Input
doGrow input = concatMap (\row -> if (or row) then [row] else [row, row]) colGrowth
    where
        colGrowth = transpose (concatMap (\row -> if (or row) then [row] else [row, row]) (transpose input))

allPairs :: [Point] -> [(Point, Point)]
allPairs [] = []
allPairs (x:xs) = zip xs (repeat x) ++ (allPairs xs)

partA :: Input -> OutputA
partA input = sum (map (\((ax, ay),(bx, by)) -> (abs (ax-bx)) + (abs (ay - by))) (allPairs (galaxyCoords)))
    where
        growth = doGrow input
        galaxyCoords = catMaybes (zipWith (\b c -> if b then Just c else Nothing) (concat growth) [(x,y) | y<-[0..(length growth)-1], x <- [0..(length (head growth))-1] ])

------------ PART B ------------
sumWithSkip :: Int -> [Int] -> [Int] -> (Point, Point) -> Int
sumWithSkip amount expandRows expandCols ((ax, ay), (bx, by)) = (abs (ax-bx)) + (abs (ay - by)) + (amount-1) * (numColsExapand+numRowsExapand)
    where
        numColsExapand = length ([(min ax bx)..(max ax bx)] `intersect` expandCols)
        numRowsExapand = length ([(min ay by)..(max ay by)] `intersect` expandRows)

partB :: Input -> OutputB
partB input = sum (map (toInteger . (sumWithSkip 1000000 expandRows expandCols)) pairs)
    where
        expandRows = catMaybes (zipWith (\b i -> if or b then Nothing else Just i) input [0..])
        expandCols = catMaybes (zipWith (\b i -> if or b then Nothing else Just i) (transpose input) [0..])
        galaxyCoords = catMaybes (zipWith (\b c -> if b then Just c else Nothing) (concat input) [(x,y) | y<-[0..(length input)-1], x <- [0..(length (head input))-1] ])
        pairs = allPairs galaxyCoords

