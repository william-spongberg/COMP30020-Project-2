-- Author: William Spongberg, Student ID: 1354263
-- Created for COMP30020, Semester 2 2024
-- This module provides functions to generate poems and haikus based on a list
-- of words and their respective syllable counts.

module Proj2 (fillInPoem, generateAllHaikus) where

import ProblemSetup (Poem, PoemMetric (..), PoemScore, distinct)
import Words (syllables)

import Data.List (delete, permutations)
import qualified Data.Map as M (Map, adjust, findWithDefault, fromList,
                                fromListWith, keys, toList)
import qualified Data.Set as S (fromList, toList)
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------------

-- | 'fillInPoem' generates all possible poems that match the given syllable
-- pattern. It takes a list of words and a list of syllable counts, and returns
-- a list of poems. Each poem is represented as a list of strings.
fillInPoem :: [String] -> [Int] -> [Poem]
fillInPoem [] _ = []
fillInPoem _ [] = []
fillInPoem words sylls = generatePoems sylls (mapWordsToSyllables words)

-- | 'generateAllHaikus' generates all valid haikus (5-7-5 syllable pattern)
-- from a list of words. It takes a list of words and returns a list of haikus,
-- each represented as a list of strings.
generateAllHaikus :: [String] -> [Poem]
generateAllHaikus [] = []
generateAllHaikus words = generateHaikus haikuSyllables syllableMap
  where
    haikuSyllables = findHaikuSyllables syllableMap
    syllableMap = mapWordsToSyllables words

-- | 'mapWordsToSyllables' converts a list of words to a map with syllables as
-- keys. It takes a list of words and returns a map from syllable counts to
-- lists of words.
mapWordsToSyllables :: [String] -> M.Map Int [String]
mapWordsToSyllables words =
  M.fromListWith (++) [(getSyllables word, [word]) | word <- words]

-- | 'findHaikuSyllables' finds all valid 5-7-5 syllable combinations. It takes
-- a map from syllable counts to lists of words and returns a list of valid
-- syllable combinations.
findHaikuSyllables :: M.Map Int [String] -> [[Int]]
findHaikuSyllables syllMap =
  mergeLists syllables5 (mergeLists syllables7 syllables5)
  where
    syllables5 = findSyllableCombos syllMap 5
    syllables7 = findSyllableCombos syllMap 7

-- | 'generateHaikus' generates haikus for each syllable combination. It takes a
-- list of syllable combinations and a map from syllable counts to lists of
-- words. It returns a list of haikus, each represented as a list of strings.
generateHaikus :: [[Int]] -> M.Map Int [String] -> [[String]]
generateHaikus haikuSyllables syllMap =
  [haiku | syllables <- haikuSyllables, haiku <- generatePoems syllables syllMap]

-- | 'generatePoems' accumulatively generates words for each syllable
-- combination. It takes a list of syllable counts and a map from syllable
-- counts to lists of words. It returns a list of poems, each represented as a
-- list of strings.
generatePoems :: [Int] -> M.Map Int [String] -> [[String]]
generatePoems [] _ = [[]]
generatePoems (s : syllables) wordMap = do
  word <- M.findWithDefault [] s wordMap
  rest <- generatePoems syllables (M.adjust (delete word) s wordMap)
  return (word : rest)

-- | 'findSyllableCombos' gets all distinct permutations of syllables that add
-- up to k. It takes a map from syllable counts to lists of words and a target
-- syllable count. It returns a list of valid syllable combinations.
findSyllableCombos :: M.Map Int [String] -> Int -> [[Int]]
findSyllableCombos syllMap k =
  S.toList
    . S.fromList
    . concatMap permutations
    . filter (canGenerateWords syllMap)
    $ filter (isSubset syllables) (partitions k)
  where
    syllables = M.keys syllMap

-- | 'canGenerateWords' checks if there exists a distinct word combination that
-- fits the syllable combination. It takes a map from syllable counts to lists
-- of words and a list of syllable counts. It returns True if a valid word
-- combination exists, otherwise False.
canGenerateWords :: M.Map Int [String] -> [Int] -> Bool
canGenerateWords syllMap syllables =
  foldr
    ( \s acc ->
        acc
          || any distinct (mapM (\s -> M.findWithDefault [] s syllMap) syllables)
    )
    False
    syllables

-- | 'mergeLists' merges lists of lists together. It takes two lists of lists
-- and returns a merged list of lists.
mergeLists :: [[e]] -> [[e]] -> [[e]]
mergeLists xs ys = [x ++ y | x <- xs, y <- ys]

-- | 'isSubset' checks that all elements of one list are in another. It takes
-- two lists and returns True if the first list is a subset of the second,
-- otherwise False.
isSubset :: (Eq e) => [e] -> [e] -> Bool
isSubset list = all (`elem` list)

-- | 'partitions' finds all unordered partitions of k. It takes an integer k and
-- returns a list of lists of integers that sum to k.
partitions :: Int -> [[Int]]
partitions 0 = [[]]
partitions k = [x : y | x <- [1 .. k], y <- partitions (k - x)]

-- | 'getSyllables' is a wrapper function to avoid Maybe Int (returns 0 if
-- Nothing). It takes a word and returns its syllable count.
getSyllables :: String -> Int
getSyllables word = fromMaybe 0 (syllables word)
