module Days.Day19 (runDay) where

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
import Control.Applicative.Combinators (between)
import Control.Applicative ((<|>))
import Prelude hiding (GT, LT)
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    workflows <- parseWorkflows
    skipSpace
    ratings <- parseRating `sepBy1'` endOfLine
    return (Map.fromList workflows, ratings)

parseWorkflows = parseWorkflow `sepBy1'` endOfLine

parseWorkflow = do
    name <- many1' (notChar '{')
    char '{'
    rules <- parseRules
    char '}'
    return (name, rules)


parseRules = (parseLTCondition <|> parseGTCondition <|> parseUnconditional) `sepBy1'` (char ',')

parseLTCondition = do 
    key <- many1' letter
    char '<' 
    num <- decimal
    char ':'
    step <- many1' letter
    return (LT key num, step)

parseGTCondition = do 
    key <- many1' letter
    char '>' 
    num <- decimal
    char ':'
    step <- many1' letter
    return (GT key num, step)    

parseUnconditional = do 
    step <- many1' letter
    return (T, step)
    
parseRating :: Parser (Map String Int)
parseRating = do
    char '{' 
    values <- parseRatingValue `sepBy1'` (char ',')
    char '}'
    return (Map.fromList values)
    where
        parseRatingValue = (,) <$> (many1' letter) <* (char '=') <*> decimal



------------ TYPES ------------
type Input = (Workflow, [Rating])
data Condition = LT String Int | GT String Int | T deriving Show
type Workflow = Map String [(Condition, String)]
type Rating = Map String Int
type RatingB = Map String (Int, Int)


type OutputA = Int

type OutputB = Int

------------ PART A ------------
processCondition :: Rating -> Condition -> Bool
processCondition rating (LT key value) = rating Map.! key < value
processCondition rating (GT key value) = rating Map.! key > value
processCondition rating T = True

processWorkflow :: Rating -> [(Condition, String)] -> String
processWorkflow rating conditions = snd (fromJust (find (\(c, s) -> processCondition rating c) conditions))

processStep :: Workflow -> Rating -> String -> Maybe (String, String)
processStep workflow rating "A" = Nothing
processStep workflow rating "R" = Nothing
processStep workflow rating key = Just (nextKey, nextKey)
    where 
        nextKey = processWorkflow rating (workflow Map.! key)
    
isAccepted :: Workflow -> String -> Rating -> Bool
isAccepted workflow "A" rating = True
isAccepted workflow "R" rating = False
isAccepted workflow key rating  = isAccepted workflow  (processWorkflow rating (workflow Map.! key)) rating
        

partA :: Input -> OutputA
partA (workflow, ratings) = sum (concatMap Map.elems accepted)
    where
        accepted = filter (isAccepted workflow "in") ratings

------------ PART B ------------
processWorkflowB :: Workflow -> RatingB -> [(Condition, String)] -> [RatingB]
processWorkflowB _ rating [] = [rating]
processWorkflowB workflow rating ((condition, "R"): xs) = case processConditionB rating condition  of
    (Just nextRating, Nothing) -> []
    (Nothing, Just negRating) -> processWorkflowB workflow negRating xs
    (Just nextRating, Just negRating) -> processWorkflowB workflow negRating xs

processWorkflowB workflow rating ((condition, next): xs) = case processConditionB rating condition  of
    (Just nextRating, Nothing) -> processWorkflowB workflow nextRating (workflow Map.! next)
    (Nothing, Just negRating) -> processWorkflowB workflow negRating xs
    (Just nextRating, Just negRating) -> (processWorkflowB workflow nextRating (workflow Map.! next)) ++ (processWorkflowB workflow negRating xs)


processConditionB :: RatingB -> Condition -> (Maybe RatingB, Maybe RatingB)
processConditionB rating (LT key value) 
    | value >= end = (Just rating, Nothing)
    | value > start = (Just (Map.insert key (start, value) rating), Just (Map.insert key (value, end) rating))
    | otherwise = (Nothing, Just rating)
    where
        (start, end) = rating Map.! key
        
processConditionB rating (GT key value) 
    | value <= start = (Just rating, Nothing)
    | value < end = (Just (Map.insert key (value+1, end) rating), Just (Map.insert key (start, value+1) rating) )
    | otherwise = (Nothing, Just rating)
    where
        (start, end) = rating Map.! key      
processConditionB rating T = (Just rating, Nothing)

calcCombos :: RatingB -> Int
calcCombos rating = product (map (\(s,e) -> e - s) (Map.elems rating))

partB :: Input -> OutputB
partB (workflow, _) = sum (map calcCombos ratings)
    where
        initRating = Map.fromList [("x", (1, 4001)), ("m", (1, 4001)), ("a", (1, 4001)), ("s", (1, 4001))]
        fullWf = Map.insert "A" [] workflow
        ratings = processWorkflowB fullWf initRating (workflow Map.! "in") 
