module Days.Day21 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser = do
    points <- (many1' (notChar '\n')) `sepBy1'` endOfLine
    return (toLayout points)

toLayout :: [String] -> Input
toLayout points = Map.fromList (zip [(x,y) | y <- [0..(length points)-1], x <- [0..(length (head points))-1]] (concat points))


------------ TYPES ------------
type Input = Map Point Char

type Point = (Int, Int)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
neighbors :: Point -> [Point]
neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

expand :: Set Point -> [Point] -> [Point]
expand map points = filter (`Set.member` map) (nub (concatMap neighbors points))

-- partA :: Input -> OutputA
partA map = length (nub (steps !! 64))
    where 
        Just (startPoint, _) = find (\(_, c) -> c == 'S') (Map.assocs map)
        keys = Set.insert startPoint (Map.keysSet (Map.filter (== '.') map))
        steps = iterate (expand keys) [startPoint]

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
