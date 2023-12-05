module Days.Day04 (runDay) where

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
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy1` endOfLine

parseLine :: Parser ([Int], [Int])
-- parseLine = (skipWhile (/= ':') ) *> char ':' *> ((,) <$> (many1 parseNumbers) <*> (skipSpace *> char '|' *> many1 parseNumbers))
parseLine = do
    string "Card"
    skipSpace
    decimal
    string ":"
    winning <- many1 parseNumbers
    skipSpace
    char '|'
    have <- many1 parseNumbers
    return (winning, have)
    where 
        parseNumbers = skipSpace >> decimal


------------ TYPES ------------
type Input = [([Int], [Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
findWinning :: ([Int], [Int]) -> [Int]
findWinning (winning, have) = nub (winning `intersect` have)

calcPoints :: [Int] -> Int
calcPoints [] = 0
calcPoints selected = 2 ^ ((length selected)-1)

partA :: Input -> OutputA
partA input = sum (map (calcPoints . findWinning)  input)

------------ PART B ------------
incrementNext :: Int -> Int -> [Int] -> [Int]
incrementNext amount count input = zipWith (+) input ((replicate count amount) ++ (repeat 0))

doCard :: (Int, [Int]) -> ([Int], [Int]) -> (Int, [Int])
doCard (count, (timesToRun: xs)) game = (count + timesToRun, futureCards)
    where
        winning = findWinning game
        futureCards = (incrementNext timesToRun) (length winning) xs


partB :: Input -> OutputB
partB input = res
    where 
        (res, _) = foldl' doCard (0, replicate (length input) 1) input
