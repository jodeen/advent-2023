module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.List as List
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
import Data.Attoparsec.Text (Parser, char, decimal, sepBy1, endOfLine, skipSpace, string)
import Data.Void
import Data.Functor (($>), (<&>))
import Control.Applicative.Combinators (between, choice)
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseGame `sepBy1` endOfLine
-- inputParser = (,) <$> ((string "Game ") $> decimal $> ":") <*> (endOfLine $> itemParser)

parseGame :: Parser Game
parseGame = do
    string "Game "
    n <- decimal
    string ":"
    rounds <- parseRound `sepBy1` (char ';')
    return (n, rounds)

parseRound :: Parser [Color]
parseRound = parseColor `sepBy1` (char ',')

parseColor :: Parser Color
parseColor = parseBlue <|> parseRed <|> parseGreen
    where
        parseBlue =  do
            skipSpace
            n <- decimal
            skipSpace
            string "blue"
            return (Blue n)   
        parseRed =  do
            skipSpace
            n <- decimal
            skipSpace
            string "red"
            return (Red n)   
        parseGreen =  do
            skipSpace
            n <- decimal
            skipSpace
            string "green"
            return (Green n)    
            --  (Red <$> string "red" <$> decimal) <|>
            --  (Green <$> string "green" <$> decimal)


------------ TYPES ------------
type Input = [Game]
type Game = (Int, [[Color]])
data Color = Blue Int | Red Int | Green Int deriving (Show)


type OutputA = Int

type OutputB = Int

------------ PART A ------------
isPossible :: Color -> Bool
isPossible (Blue n) = n <= 14
isPossible (Red n) = n <= 12
isPossible (Green n) = n <= 13

isPossibleRound :: [Color] -> Bool
isPossibleRound = List.all isPossible

isPossibleGame :: Game -> Bool
isPossibleGame (_, rounds) = List.all isPossibleRound rounds

partA :: Input -> OutputA
partA games = sum (map fst (filter isPossibleGame games))

------------ PART B ------------
extractValue :: Color -> Int
extractValue (Red x) = x
extractValue (Green x) = x
extractValue (Blue x) = x

isRed (Red _) = True
isRed _ = False
isGreen (Green _) = True
isGreen _ = False
isBlue (Blue _) = True
isBlue _ = False

powerGame :: Game -> Int
powerGame (_, rounds) = maxRed * maxBlue * maxGreen
    where
        maxRed =  maximum (map extractValue (filter isRed (concat rounds)))
        maxGreen =  maximum (map extractValue (filter isGreen (concat rounds)))
        maxBlue =  maximum (map extractValue (filter isBlue (concat rounds)))

partB :: Input -> OutputB
partB input = sum (map powerGame input)

