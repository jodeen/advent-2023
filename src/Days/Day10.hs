module Days.Day10 (runDay) where

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
import Data.Char (isSpace)
import Control.Monad (filterM)
import GHC.IO (unsafePerformIO)
import Util.Util (chunksOf)
{- ORMOLU_ENABLE -}
{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1' (notChar '\n')) `sepBy1'` endOfLine

------------ TYPES ------------
type Input = [String]
type Point = (Int, Int)
type Coords = Map Point Char

type OutputA = Int

type OutputB = Int

------------ PART A ------------
toCoordsMap :: [String] ->Coords
toCoordsMap input = Map.fromList (zip coords (concat input))
    where
        coords = [(x,y) | y<-[0..(length input)-1], x <- [0..(length (head input))-1] ]

initPipes :: Coords -> Point -> [Point]
initPipes coords (x,y) = catMaybes [north, south, east, west]
    where
        north = findMapValue coords (x,y-1) "|F7"
        south = findMapValue coords (x,y+1) "|LJ"
        east = findMapValue coords (x+1,y) "-J7"
        west = findMapValue coords (x-1,y) "-LF"

findMapValue :: Coords -> Point -> String -> Maybe Point
findMapValue coords p values = if (isJust value && (fromJust value) `elem` values) then Just p else Nothing
    where
        value = Map.lookup p coords

connections :: Point -> Char -> [Point]
connections (x,y) '|' = [(x,y-1), (x,y+1)]
connections (x,y) '-' = [(x-1,y), (x+1,y)]
connections (x,y) 'L' = [(x,y-1), (x+1,y)]
connections (x,y) 'J' = [(x,y-1), (x-1,y)]
connections (x,y) '7' = [(x,y+1), (x-1,y)]
connections (x,y) 'F' = [(x,y+1), (x+1,y)]

doStep :: Coords -> Point -> (Point, Point) -> Maybe (Point, (Point, Point))
doStep coords endPoint (prevPoint, currentPoint) = if currentPoint == endPoint then Nothing else Just (nextPoint, (currentPoint, nextPoint))
    where
        nextPoint = head (filter (/= prevPoint) (connections currentPoint (coords Map.! currentPoint)))

walkPath :: Coords -> Point -> [Point]
walkPath coords start = next : unfoldr (doStep coords start) (start, next)
    where
        next = head (initPipes coords start)


partA :: Input -> OutputA
partA input = (length path) `div` 2
    where
        coords = toCoordsMap input
        Just start = fmap fst (find (\(_,c) -> c == 'S') (Map.assocs coords))
        path = walkPath coords start

------------ PART B ------------
-- a point is inside the cycle if there's an odd number of vertical lines to the left (or right)
isInside :: [Point] -> Point -> Bool
isInside path (col, row) = ((col, row) `notElem` path) && odd (length rowPairs)
    where
        pairs = zip ((last path) : path) path
        -- first we normalize the pairs by ordering them by increasing (x,y) values 
        -- that way we only need to check the ending values of the path
        normPairs = map (\(a,b) -> if a<=b then (a,b) else (b,a)) (filter (\((_, ay), (_,by)) -> ay /= by) pairs)
        rowPairs = filter (\(_, (x,y)) -> y == row && x <= col) normPairs



partB :: Input -> OutputB
partB input =  pointCount
    where
        coords = toCoordsMap input
        Just start = fmap fst (find (\(_,c) -> c == 'S') (Map.assocs coords))
        path = walkPath coords start

        allPoints = [(x,y) | y<-[0..(length input)-1], x <- [0..(length (head input))-1] ]
        pointCount = length (filter id (map (isInside path) allPoints))


