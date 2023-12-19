module Days.Day17 (runDay) where

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
import Data.Attoparsec.Text hiding (D)
import Data.Void
import Data.Char (digitToInt)
import Algorithm.Search (dijkstra)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    points <- (many1' (notChar '\n')) `sepBy1'` endOfLine
    return (Map.map digitToInt (toLayout points))


-- toLayout :: [String] -> Input
toLayout points = Map.fromList (zip [(x,y) | y <- [0..(length points)-1], x <- [0..(length (head points))-1]] (concat points))


------------ TYPES ------------
type Input = Map (Int, Int) Int
data Dir = U | D | L | R deriving (Show, Eq, Ord)
type State = ((Int, Int), Dir, Int)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
turns ((x,y), U, _) = [((x-1, y), L, 1), ((x+1, y), R, 1)]
turns ((x,y), D, _) = [((x-1, y), L, 1), ((x+1, y), R, 1)]
turns ((x,y), L, _) = [((x, y+1), D, 1), ((x, y-1), U, 1)]
turns ((x,y), R, _) = [((x, y+1), D, 1), ((x, y-1), U, 1)]

continueDir ((x,y), U, c) = ((x, y-1), U, c+1)
continueDir ((x,y), D, c) = ((x, y+1), D, c+1)
continueDir ((x,y), L, c) = ((x-1, y), L, c+1)
continueDir ((x,y), R, c) = ((x+1, y), R, c+1)



nextNodes :: Input -> State -> [State]
nextNodes layout (p, dir, count) 
    | count == 3 = filter (\(point, _, _) -> Map.member point layout) turned
    | otherwise = filter (\(point, _, _) -> Map.member point layout) (continueDir (p, dir, count) : turned)
    where
        turned = turns (p, dir, count)

cost :: Input -> State -> State -> Int
cost layout _ (p, _, _) = layout Map.! p




doSearch layout = dijkstra (nextNodes layout) (cost layout)  (\(p, _, _) -> p == (maxX, maxY)) ((0, 0), R, 1)
    where
        maxX = maximum (map fst (Map.keys layout))
        maxY = maximum (map snd (Map.keys layout))

-- doSearch' layout = dijkstra (nextNodes layout) (cost layout) ()  (\(p, _, _) -> p == (maxX, maxY)) ((0, 0), R, 1)
--     where
--         maxX = maximum (map fst (Map.keys layout))
--         maxY = maximum (map snd (Map.keys layout))        

-- partA :: Input -> OutputA
partA input = cost
    where
        Just (cost, path) = doSearch input
        maxX = maximum (map fst (Map.keys input))
        maxY = maximum (map snd (Map.keys input))        

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
