module Days.Day05 (runDay) where

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
import Data.Text (pack)
import Data.Function (on)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    seedsList <- seedParser
    endOfLine
    seedToSoilMap <- mapParser "seed-to-soil"
    endOfLine
    soilToFertilizerMap <- mapParser "soil-to-fertilizer"
    endOfLine
    fertilizerToWaterMap <- mapParser "fertilizer-to-water"
    endOfLine
    waterToLightMap <- mapParser "water-to-light"
    endOfLine
    lightToTempMap <- mapParser "light-to-temperature"
    endOfLine
    tempToHumidityMap <- mapParser "temperature-to-humidity"
    endOfLine
    humidityToLocationMap <- mapParser "humidity-to-location"
    return State {seeds=seedsList
                  , seedToSoil=seedToSoilMap
                  , soilToFertilizer=soilToFertilizerMap
                  , fertilizerToWater=fertilizerToWaterMap
                  , waterToLight=waterToLightMap
                  , lightToTemp=lightToTempMap
                  , tempToHumidity=tempToHumidityMap
                  , humidityToLocation=humidityToLocationMap  
                  }


seedParser :: Parser [Integer]
seedParser = string "seeds: " *> (decimal `sepBy1'` space)

mapParser :: String -> Parser RangeMap
mapParser heading = do
    endOfLine
    string (pack heading)
    string " map:"
    endOfLine    
    ranges <- lineParser `sepBy1` endOfLine
    return (sortOn (\(a,_,_) -> a) ranges)

lineParser :: Parser Range
lineParser = do
            destinationStart <- decimal
            skipSpace
            sourceStart <- decimal
            skipSpace
            rangeLength <- decimal
            return (sourceStart, destinationStart, rangeLength)

------------ TYPES ------------
type Input = State
data State = State {seeds :: [Integer]
                    , seedToSoil :: RangeMap
                    , soilToFertilizer :: RangeMap
                    , fertilizerToWater :: RangeMap
                    , waterToLight :: RangeMap
                    , lightToTemp :: RangeMap
                    , tempToHumidity :: RangeMap
                    , humidityToLocation :: RangeMap
                    } deriving (Show)
type Range = (Integer, Integer, Integer)
type RangeMap = [Range]
type SeedRange = (Integer, Integer)  -- (Start, end) of the seed range

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
isInRange :: Integer -> Range -> Bool
isInRange num (fromStart, _, count) = num >= fromStart && num < (fromStart + count)

doMap :: RangeMap -> Integer -> Integer
doMap ranges num = maybe num (\(fromStart, toStart, _) -> num - fromStart + toStart) (find (isInRange num) ranges)

partA :: Input -> OutputA
partA state = minimum locations
    where 
        mapChain = foldr1 (.) (map doMap (reverse [
            seedToSoil state
            , soilToFertilizer state
            , fertilizerToWater state
            , waterToLight state
            , lightToTemp state
            , tempToHumidity state
            , humidityToLocation state
            ]))
        locations = map mapChain (seeds state)


------------ PART B ------------
splitSeedRange :: Range -> SeedRange -> [SeedRange]
splitSeedRange (rangeStart, _, count) (seedStart, seedEnd)  
        | (seedEnd < rangeStart) || (seedStart >= rangeEnd)  = [(seedStart, seedEnd)] 
        | (seedStart >= rangeStart) && (seedEnd <= rangeEnd) = [(seedStart, seedEnd)]
        | (seedStart < rangeStart) && (seedEnd <= rangeEnd) = [(seedStart, rangeStart), (rangeStart, seedEnd)]
        | (seedStart >= rangeStart) && (seedEnd > rangeEnd) = [(seedStart, rangeEnd), (rangeEnd, seedEnd)]
        | (seedStart < rangeStart) && (seedEnd > rangeEnd) = [(seedStart, rangeStart), (rangeStart, rangeEnd), (rangeEnd, seedEnd)]
    where 
        rangeEnd = rangeStart + count 

splitSeedByMap :: RangeMap -> SeedRange -> [SeedRange]
splitSeedByMap rangeMap seedRange  = foldl' (\seeds range -> concatMap (splitSeedRange range) seeds ) [seedRange] rangeMap

applyMapToSeedRange :: SeedRange -> Range -> Maybe SeedRange
applyMapToSeedRange (seedStart, seedEnd) (fromStart, toStart, count) = 
    if (seedStart >= fromStart) && (seedEnd <= (fromStart + count))
    then Just (toStart + seedStart - fromStart, toStart + seedEnd - fromStart)
    else Nothing 
        
applyFullMaptoSeedRange :: RangeMap -> SeedRange -> SeedRange
applyFullMaptoSeedRange rangeMap seedRange = fromMaybe seedRange (listToMaybe (catMaybes (map (applyMapToSeedRange seedRange) rangeMap)))

applySeedsToMap :: RangeMap -> [SeedRange] -> [SeedRange]
applySeedsToMap rangeMap seeds = map (applyFullMaptoSeedRange rangeMap) normalizedSeeds
    where
        normalizedSeeds = filter (\(a,b) -> a /= b) (concatMap (splitSeedByMap rangeMap) seeds)

pairsToRange :: [Integer] -> [(Integer, Integer)]
pairsToRange (a:b:xs) = (a,a+b) : pairsToRange xs
pairsToRange [] = []

sampleState = State {seeds = [79,14,55,13], seedToSoil = [(50,52,48),(98,50,2)], soilToFertilizer = [(0,39,15),(15,0,37),(52,37,2)], fertilizerToWater = [(0,42,7),(7,57,4),(11,0,42),(53,49,8)], waterToLight = [(18,88,7),(25,18,70)], lightToTemp = [(45,81,19),(64,68,13),(77,45,23)], tempToHumidity = [(0,1,69),(69,0,1)], humidityToLocation = [(56,60,37),(93,56,4)]}

partB :: Input -> OutputB
partB state =  fst (minimumBy (compare `on` fst) mappedRanges)
    where
        mapChain = foldr1 (.) (map applySeedsToMap (reverse [
            seedToSoil state
            , soilToFertilizer state
            , fertilizerToWater state
            , waterToLight state
            , lightToTemp state
            , tempToHumidity state
            , humidityToLocation state
            ]))
        seedRanges = pairsToRange (seeds state)
        mappedRanges = mapChain seedRanges
