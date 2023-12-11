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

type OutputB = Void

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
toInOutRow :: String -> [Bool]
toInOutRow row = tail (scanl swapInOutRowChar False row)

swapInOutRowChar :: Bool -> Char -> Bool
swapInOutRowChar state char = if (char `elem` ['|', 'L', 'J', '7', 'F']) then (not state) else state

toInOutCol :: String -> [Bool]
toInOutCol row = tail (scanl swapInOutColChar False row)

swapInOutColChar :: Bool -> Char -> Bool
swapInOutColChar state char = if (char `elem` ['-', 'L', 'J', '7', 'F']) then (not state) else state

inOutMap path input = zipWith3 (\point row col -> (point `notElem` path) && (col && row)) points rowwise colwise
    where
        rowwise = concatMap toInOutRow input
        colwise = concat (transpose (map toInOutCol (transpose input)))
        points = [(x,y) | y<-[0..(length input)-1], x <- [0..(length (head input))-1] ]

findProperS :: Coords -> Point -> Char
findProperS coords (x,y) = case (north, south, east, west) of 
    (True, True, False, False) -> '|'
    (False, False, True, True) -> '-'
    (True, False, True, False) -> 'L'
    (True, False, False, True) -> 'J'
    (False, True, False, True) -> '7'
    (False, True, True, False) -> 'F'
    where
        north = isJust (findMapValue coords (x,y-1) "|F7")
        south = isJust (findMapValue coords (x,y+1) "|LJ")
        east = isJust (findMapValue coords (x+1,y) "-J7")
        west = isJust (findMapValue coords (x-1,y) "-LF")


replace :: [String] -> (Int, Int) -> Char -> [String]
replace input (column, row) c = headRows ++ ((headCols ++ (c : tailCols)) : tailRows)
    where
        (headRows, rowData:tailRows) = splitAt row input
        (headCols, colData:tailCols) = splitAt column rowData

removeNonPath :: [Point] -> [String] -> [String]
removeNonPath path input = chunksOf (length (head input)) (map (\(p, c) -> if p `notElem` path then '.' else c) (zip coords (concat input)))
    where
        coords = [(x,y) | y<-[0..(length input)-1], x <- [0..(length (head input))-1] ]


-- isVertical :: Int -> (Point, Point) -> Bool
-- isVertical num ((ax, ay), (bx, by)) = ((by == num) ) && (ay /= by)

-- -- verticalLines :: [Point] -> Int -> [Point]
-- verticalLines path row = filter (isVertical row) pairs
--     where
--         pairs = zip ((last path) : path) path



-- partB :: Input -> OutputB
partB input =  unsafePerformIO (putStr (unlines inOutChunked))
    where
        coords = toCoordsMap input
        Just start = fmap fst (find (\(_,c) -> c == 'S') (Map.assocs coords))
        path = walkPath coords start

        properS = findProperS coords start
        properInput = removeNonPath path (replace input start properS)
        inOut = inOutMap path properInput
        inOutChunked =  chunksOf (length (head input)) (map (\b -> if b then '*' else '.') inOut)
        
