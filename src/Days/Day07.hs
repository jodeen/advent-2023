module Days.Day07 (runDay) where

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
import Data.Char (digitToInt)
import Control.Applicative ((<|>))
import Data.Ord (comparing, Down (Down))
{- ORMOLU_ENABLE -}
{- HLINT ignore "Redundant bracket" -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = lineParser `sepBy1'` endOfLine
    where
        lineParser = do
            hand <- handParser 
            skipSpace
            bid <- decimal
            return (hand, bid)

handParser :: Parser [Int]
handParser = many1 ((digitToInt <$> digit) <|> readChar)
    where
        readChar = do
            c <- letter
            return (case c of 
                'A' -> 14
                'K' -> 13
                'Q' -> 12
                'J' -> 11
                'T' -> 10)

------------ TYPES ------------
type Input = [([Int], Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

countInstances :: [Int] -> [(Int, Int)]
countInstances hand = sortBy (comparing Down) (map (\s -> (length s, head s)) (group (sort hand)))

isFiveOfKind :: [Int] -> Bool
isFiveOfKind hand = (length (nub hand)) == 1

isFourOfKind :: [Int] -> Bool
isFourOfKind hand = case (head (countInstances hand)) of
    (4, _) -> True
    _ -> False

isFullHouse :: [Int] -> Bool
isFullHouse hand = case (countInstances hand) of
    ((3, _):(2, _):_) -> True
    _ -> False    


isThreeOfKind :: [Int] -> Bool
isThreeOfKind hand = case (countInstances hand) of
    ((3, _):(1, _):_) -> True
    _ -> False        

isTwoPair :: [Int] -> Bool
isTwoPair hand = case (countInstances hand) of
    ((2, _):(2, _):_) -> True
    _ -> False    

isPair :: [Int] -> Bool
isPair hand = (length (nub hand)) == 4

handStrength :: ([Int], bid) -> (Int, [Int], bid)
handStrength (hand, bid) 
    | isFiveOfKind hand = (7, hand, bid)
    | isFourOfKind hand = (6, hand, bid)
    | isFullHouse hand = (5, hand, bid)
    | isThreeOfKind hand = (4, hand, bid)
    | isTwoPair hand = (3, hand, bid)
    | isPair hand = (2, hand, bid)
    | otherwise = (1, hand, bid)


partA :: Input -> OutputA
partA input = sum (zipWith (\(_, _, bid) rank -> rank * bid) sortedHands [1..])
    where
        sortedHands = sort (map handStrength input)

------------ PART B ------------

jackToJoker :: ([Int], Int) -> ([Int], Int)
jackToJoker (hand, bid) = (map (\c -> if c == 11 then 0 else c) hand, bid)

isFiveOfKind' :: [Int] -> Bool
isFiveOfKind' hand =  (length (nub hand)) == 1 ||
    case (head (countInstances noJokers)) of
        (5, _) -> True
        (4, _) -> jokerCount == 1 
        (3, _) -> jokerCount == 2
        (2, _) -> jokerCount == 3 
        (1, _) -> jokerCount == 4 

        _ -> False
    where 
        jokerCount = length (filter (== 0) hand)
        noJokers = filter (> 0) hand

isFourOfKind' :: [Int] -> Bool
isFourOfKind' hand = case (head (countInstances noJokers)) of
        (4, _) -> True
        (3, _) -> jokerCount == 1
        (2, _) -> jokerCount == 2 
        (1, _) -> jokerCount == 3 
        _ -> False
    where 
        jokerCount = length (filter (== 0) hand)
        noJokers = filter (> 0) hand

isThreeOfKind' :: [Int] -> Bool
isThreeOfKind' hand = case (head (countInstances noJokers)) of
        (3, _) -> True
        (2, _) -> jokerCount == 1
        (1, _) -> jokerCount == 2 
        _ -> False
    where 
        jokerCount = length (filter (== 0) hand)
        noJokers = filter (> 0) hand

isPair' :: [Int] -> Bool
isPair' hand = case (head (countInstances noJokers)) of
        (2, _) -> True
        (1, _) -> jokerCount == 1
        _ -> False
    where 
        jokerCount = length (filter (== 0) hand)
        noJokers = filter (> 0) hand

isTwoPair' :: [Int] -> Bool
isTwoPair' hand = case (countInstances noJokers) of
        ((2, _):(2, _):_) -> True
        ((2, _):(1, _):_) -> jokerCount == 1
        _ -> False
    where 
        jokerCount = length (filter (== 0) hand)
        noJokers = filter (> 0) hand

isFullHouse' :: [Int] -> Bool
isFullHouse' hand = case (countInstances noJokers) of
        ((3, _):(2, _):_) -> True
        ((2, _):(2, _):_) -> jokerCount == 1
        _ -> False
    where 
        jokerCount = length (filter (== 0) hand)
        noJokers = filter (> 0) hand

handStrength' :: ([Int], bid) -> (Int, [Int], bid)
handStrength' (hand, bid) 
    | isFiveOfKind' hand = (7, hand, bid)
    | isFourOfKind' hand = (6, hand, bid)
    | isFullHouse' hand = (5, hand, bid)
    | isThreeOfKind' hand = (4, hand, bid)
    | isTwoPair' hand = (3, hand, bid)
    | isPair' hand = (2, hand, bid)
    | otherwise = (1, hand, bid)

partB :: Input -> OutputB
partB input = sum (zipWith (\(_, _, bid) rank -> rank * bid) sortedHands [1..])
    where
        sortedHands = sort (map (handStrength' . jackToJoker) input)
