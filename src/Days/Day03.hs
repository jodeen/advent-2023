module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
-- import qualified Data.List as List
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
import Control.Applicative ((<|>))
import qualified Data.Text as Text
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

{- HLINT ignore "Redundant bracket" -}


runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy1` endOfLine

parseLine :: Parser [Item]
parseLine = many1 parseItem

parseItem :: Parser Item
parseItem = parseDecimal <|> parseEmpty <|> parseChar
    where
        parseDecimal = Candidate <$> decimal
        parseEmpty = (char '.') $> Empty
        parseChar = Symbol <$> (notChar '\n')

------------ TYPES ------------
type Input = [[Item]]
data Item = Candidate Int | Empty | Symbol Char deriving (Show)
type Point = (Int, Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------

lineToPositions :: [Item] -> Int -> [(Point, Item)]
lineToPositions items row = tail (scanl calcPos ((-1, row), Empty) items)

calcPos :: (Point, Item) -> Item -> (Point, Item)
calcPos ((col, row), (Candidate old)) item = ((col + (length (show old)), row), item)
calcPos ((col, row), _) item = ((col + 1, row), item)

isCandidate :: Item -> Bool
isCandidate (Candidate _) = True
isCandidate _ = False

isSymbol :: Item -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

neighbors :: Point -> [Point]
neighbors (col, row) = [(col-1, row-1), (col, row-1), (col+1, row-1),
                        (col-1, row), (col+1, row),
                        (col-1, row+1), (col, row+1), (col+1, row+1)]

candidateNeighbors :: (Point, Item) -> [Point]
candidateNeighbors ((col, row), Candidate c) = nub (concatMap (neighbors . (\i -> (col+i, row))) [0..((length (show c))-1)])

calcPartValue :: (Point, Item) -> Int
calcPartValue (_, Candidate x) = x

isPartNum :: [Point] -> (Point, Item) -> Bool
isPartNum symbolPoints (point, Candidate c) = any (`elem` symbolPoints) n
    where 
        n = candidateNeighbors (point, Candidate c)

partNums :: [(Point, Item)] -> [(Point, Item)]
partNums positions = filter (isPartNum symbolPoints) candidates
    where
        candidates = filter (\(_, c) -> isCandidate c) positions
        symbolPoints = map fst (filter (\(_, i) -> isSymbol i) positions)
    

partA :: Input -> OutputA
partA input = sum (map calcPartValue (partNums positions))
    where
        positions = concat (zipWith lineToPositions input [0..])

------------ PART B ------------
isGearCandidate :: Item -> Bool
isGearCandidate (Symbol '*') = True
isGearCandidate _ = False

isGearAdjacent :: (Point, Item) -> (Point, Item) -> Bool
isGearAdjacent (gearPoint, _) part = gearPoint `elem` (candidateNeighbors part)

gearRatio :: [(Point, Item)] -> (Point, Item) -> Int
gearRatio parts gearCandidate = if (length adjacentParts == 2) then calcPartValue (head adjacentParts) * calcPartValue (last adjacentParts) else 0
    where 
        adjacentParts = filter (isGearAdjacent gearCandidate) parts

partB :: Input -> OutputB
partB input = sum  (map (gearRatio parts) gearCandidates)
    where
        positions = concat (zipWith lineToPositions input [0..])
        gearCandidates = filter (\(_, i) -> isGearCandidate i) positions
        parts = partNums positions
