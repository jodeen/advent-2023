module Days.Day15 (runDay) where

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
import Data.Char (ord)
import Data.Functor (($>))
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseSegment `sepBy1'` (char ',')
    where
        parseSegment = parseAdd <|> (Rem <$> (many1' letter) <* (char '-'))
        parseAdd = do
            box <- many1' letter
            char '='
            Add box <$> decimal

------------ TYPES ------------
type Input = [Segment]
data Segment = Rem String | Add String Int deriving Show

type OutputA = Int

type OutputB = Void

------------ PART A ------------
dohashSegment :: Segment -> Int
dohashSegment (Add s i) = doHash (s ++ "=" ++ (show i))
dohashSegment (Rem s) = doHash (s ++ "-")

doHash :: String -> Int
doHash = foldl' (\h c -> mod (17 * (h + (ord c))) 256) 0

partA :: Input -> OutputA
partA input = sum (map dohashSegment input)

------------ PART B ------------
replace :: (String, Int) -> [(String, Int)] -> [(String, Int)]
replace  (key, value) lenses = l ++ if (null r) then [(key, value)] else ((key, value) : tail r)
    where 
        (l, r) = span (\(k,_) -> k /= key) lenses

focalBox :: (Int, [(String, Int)]) -> Int
focalBox (boxNum, items) = (boxNum + 1) * sum (zipWith (\(_, f) l -> f * l) items [1..])


handleSegment :: Map Int [(String, Int)] -> Segment -> Map Int [(String, Int)]
handleSegment boxes (Rem key) = Map.adjust (filter (\(k, _) -> k /= key)) (doHash key) boxes
handleSegment boxes (Add key value) = Map.insertWith (\[new] list -> replace new list) (doHash key) [(key, value)] boxes

-- partB :: Input -> OutputB
partB input = sum (map focalBox (Map.assocs boxes))
    where
        boxes = foldl' handleSegment Map.empty input
