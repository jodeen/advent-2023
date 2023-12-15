module Days.Day12 (runDay) where

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
import Control.Arrow (ArrowChoice(left))
import Data.Either (lefts)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy1'` endOfLine
    where
        parseLine = do
            conditions <- many1' (notChar ' ')
            skipSpace
            condGroups <- decimal `sepBy1'` (char ',')
            return (conditions, condGroups)


------------ TYPES ------------
type Input = [(String, [Int])]

type OutputA = Int

type OutputB = Void

------------ PART A ------------
generatePermutations :: String -> [String]
generatePermutations ('?':xs) = map ('.' :) (generatePermutations xs) ++ map ('#' :) (generatePermutations xs)
generatePermutations (c:xs) = map (c :) (generatePermutations xs)
generatePermutations [] = [[]]

toGroups :: String -> [Int]
toGroups string = map length (filter (\g -> (head g) /= '.') (group string))

isValidArrangement :: [Int] -> String  -> Bool
isValidArrangement groups s = groups == toGroups s

countValid :: (String, [Int]) -> Int
countValid (string, groups) = length (filter (isValidArrangement groups) (generatePermutations string))

partA :: Input -> OutputA
partA input = sum (map countValid input)


------------ PART B ------------
isPotentialPrefix :: [Int] -> String -> Char -> Bool
isPotentialPrefix groups prefix '.' = (toGroups prefix) `isPrefixOf` groups
isPotentialPrefix groups prefix '#' = (init newGroups) `isPrefixOf` groups && (length groups >= length newGroups) && ((groups !! (length newGroups - 1)) >= (last newGroups))
    where
        newGroups = toGroups (prefix ++ ['#'])
        groupPrefix = take (length newGroups - 1) groups


generatePermutationsB :: [Int] -> String -> String -> [String]
generatePermutationsB groups prefix [] = if (isValidArrangement groups prefix) then [prefix] else []
generatePermutationsB groups prefix ('?':xs) = leftSide ++ rightSide
    where
        newPrefix = []
        leftSide = if isPotentialPrefix groups prefix '.' then generatePermutationsB groups (prefix ++ ['.']) xs else []
        rightSide = if isPotentialPrefix groups prefix '#' then generatePermutationsB groups (prefix ++ ['#']) xs else []
generatePermutationsB groups prefix (c:xs) = if isPotentialPrefix groups prefix c then generatePermutationsB groups (prefix ++ [c]) xs else []

unfoldValue :: [a] -> [a]
unfoldValue input = concat (replicate 5 input)

countValidB :: (String, [Int]) -> Int
countValidB (string, groups)= length ( generatePermutationsB newGroups [] newString)
    where 
        newGroups = concat (replicate 5 groups)
        newString = intercalate "?" (replicate 5 string)

-- partB :: Input -> OutputB
partB input =   sum (map countValidB input)
    