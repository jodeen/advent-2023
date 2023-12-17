module Days.Day16 (runDay) where

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
import Data.Attoparsec.Text hiding (take, D)
import Data.Void
{- ORMOLU_ENABLE -}
{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    points <- (many1' (notChar '\n')) `sepBy1'` endOfLine
    return (toLayout points)

toLayout :: [String] -> Input
toLayout points = Map.fromList (zip [(x,y) | y <- [0..(length points)-1], x <- [0..(length (head points))-1]] (concat points))

------------ TYPES ------------
type Input = Map Point Char

type Point = (Int, Int)
data Dir = U | D | L | R deriving (Show, Eq, Ord)
type Visit = (Point, Dir)


type OutputA = Int

type OutputB = Int

------------ PART A ------------

doStep :: Input -> (Set Visit, [Visit]) -> (Set Visit, [Visit])
doStep layout (visited, beams) = (newVisited, newBeams)
    where
        newVisited = Set.union visited (Set.fromList (filter (\(p, _) -> Map.member p layout) beams))
        newBeams = filter (`Set.notMember` newVisited) (nub (concatMap (doVisit layout) beams))

doMove :: Visit -> Visit
doMove ((x,y), U) = ((x,y-1), U)
doMove ((x,y), D) = ((x,y+1), D)
doMove ((x,y), L) = ((x-1,y), L)
doMove ((x,y), R) = ((x+1,y), R)


doVisit :: Input -> Visit -> [Visit]
doVisit layout ((x,y), dir) = case (Map.lookup (x,y) layout, dir) of 
    (Nothing, _) -> []
    (Just '-', U) -> [doMove ((x,y), L), doMove ((x,y), R)]
    (Just '-', D) -> [doMove ((x,y), L), doMove ((x,y), R)]
    (Just '|', L) -> [doMove ((x,y), U), doMove ((x,y), D)]
    (Just '|', R) -> [doMove ((x,y), U), doMove ((x,y), D)]
    (Just '/', R) -> [doMove ((x,y), U)]
    (Just '/', L) -> [doMove ((x,y), D)]
    (Just '/', U) -> [doMove ((x,y), R)]
    (Just '/', D) -> [doMove ((x,y), L)]
    (Just '\\', R) -> [doMove ((x,y), D)]
    (Just '\\', L) -> [doMove ((x,y), U)]
    (Just '\\', U) -> [doMove ((x,y), L)]
    (Just '\\', D) -> [doMove ((x,y), R)]
    _ -> [doMove ((x,y), dir)]

calcEnergy input startPoint = length (nub (map fst (Set.toList visited) ))
    where 
        (visited, _) = head (dropWhile (\(_, path) -> not (null path)) (iterate (doStep input) (Set.empty, [startPoint])))


partA :: Input -> OutputA
partA input = calcEnergy input ((0, 0), R)

------------ PART B ------------
partB :: Input -> OutputB
partB input = maximum (map (calcEnergy input) startPoints)
    where 
        width = maximum (map fst (Map.keys input))
        height = maximum (map snd (Map.keys input))
        startPoints = concat ([[((0,y), R), ((width, y), L)] | y <- [0..(height-1)]] ++
                [[((x,0), R), ((x, height), L)] | x <- [0..(width-1)]])
