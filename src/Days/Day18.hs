module Days.Day18 (runDay) where

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
import Control.Applicative.Combinators (between)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = lineParser `sepBy1'` endOfLine
    where
        lineParser = do
            c <- anyChar
            skipSpace
            num <- decimal
            skipSpace
            color <- between (string "(#") (char ')') hexadecimal
            return (c, num, color)

------------ TYPES ------------
type Input = [Move]
data Dir = U | D | L | R deriving Show
type Move = (Char, Int, Int)
type Coords = [(Int, Int)]
type State = (Int, Int)

type OutputA = Void

type OutputB = Void

------------ PART A ------------

toCoords :: [State] -> Move -> [State]
toCoords ((x,y): xs) ('U', amount, _) = (reverse [(x, y - offset) | offset <- [1..amount]]) ++ ((x,y): xs)
toCoords ((x,y): xs) ('D', amount, _) = (reverse [(x, y + offset) | offset <- [1..amount]]) ++ ((x,y): xs)
toCoords ((x,y): xs) ('L', amount, _) = (reverse [(x - offset, y) | offset <- [1..amount]]) ++ ((x,y): xs)
toCoords ((x,y): xs) ('R', amount, _) = (reverse [(x + offset, y) | offset <- [1..amount]]) ++ ((x,y): xs)

findInternal :: [Int] -> Int
findInternal items = length (filter (isInside items) [0..maxValue])
    where
        maxValue = maximum items

isInside :: [Int] -> Int -> Bool
isInside items num = odd (length (filter (< num) items)) && (num `notElem` items)

-- partA :: Input -> OutputA
partA input = path
-- partA input = sum (map findInternal (Map.elems cols) ) + length (nub path)
-- partA input = (filter (isInside (cols Map.! 1)) [0..9])
    where
        path = foldl' toCoords [(0,0)] input
        cols = Map.fromListWith (++) (map (\(a,b) -> (a, [b])) path)
        maxX = maximum (map fst path)
        minX = minimum (map fst path)
        maxY = maximum (map snd path)
        minY = minimum (map snd path)     

   

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
