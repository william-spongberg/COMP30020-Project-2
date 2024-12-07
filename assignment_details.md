# Algorithmic Poet

This project tasks you with developing an algorithmic poet, which offsets its lack of creativity with comprehensiveness.

## Haikus

A haiku is a short poem consisting of three lines, which must have the number of syllables 5, 7, 5 respectively. For this project, we will be ignoring the more aesthetic requirements of a haiku—namely the 'cutting word' and the seasonal theme—and focusing just on the combinatorial requirement.

We use the word-to-syllables data from the MRC Psycholinguistic Database to determine the number of syllables in each word.¹ You have been provided with the function:

```haskell
syllables :: String -> Maybe Int
```

in the module `Words`, which looks up the number of syllables for the given word in this database, and:

```haskell
wordList :: [String]
```

which is the list of all words in the database. Note: All the words provided to your generation function will be from this list, and thus `syllables` will succeed (return a non-Nothing value) for them.

Haikus will be represented using the type:

```haskell
type Poem = [String]
```

which simply represents the words in the poem.

The MRC Database contains multiple number-of-syllables for some words, because those words have multiple pronunciations. The `syllables` function takes the minimum of those values. You only need to consider this minimum value. Indeed, you should not need to write code to parse the `word_to_syllables.txt` file yourself.

## Your Tasks

You need to implement the following functions:

The function:

```haskell
fillInPoem :: [String] -> [Int] -> [Poem]
```

that takes a wordlist and a list of number of syllables, and creates all poems with words drawn from the wordlist where the number of syllables of the words in the poem exactly matches the value in the number-of-syllables list, in the order of that list. We require every word in the poem be distinct.

As an example:

```haskell
fillInPoem ["flowering", "jacaranda", "photosynthesis"] [3,4,5]
```

should generate:

```haskell
[["flowering", "jacaranda", "photosynthesis"]]
```

and:

```haskell
fillInPoem ["cabinet", "flowering", "jacaranda", "photosynthesis"] [3,4,5]
```

should generate:

```haskell
[["flowering","jacaranda","photosynthesis"],
 ["cabinet","jacaranda","photosynthesis"]]
```

Note that the words filled in by `fillInPoem` should be exact matches of the number of syllables, for example:

```haskell
fillInPoem ["red", "banksia", "flowering"] [3]
```

should produce:

```haskell
[["flowering"]]
```

and should not produce sequences of words that sum to the number of syllables:

```haskell
-- ✗ incorrect behaviour
[["red", "banksia"], ["banksia", "red"], ["flowering"]]
```

The function:

```haskell
generateAllHaikus :: [String] -> [Poem]
```

that generates all (good) haikus, given a wordlist from which all words in the poem must be drawn.

## A Note on Efficiency

This task will most likely require that you sort the wordlist into buckets according to the number of syllables the word has. Doing this with a plain list is inefficient, because it would require an \(O(n)\) traversal (in the length of the list) every time you wish to find a certain element.

We suggest that you use the Haskell `containers` library, included with Grok. In particular, the module `Data.Map` provides an efficient implementation of a BTree Map. This module can be imported with the statement:

```haskell
import Data.Map
```

but we suggest instead performing a qualified import:

```haskell
import qualified Data.Map as M
```

which namespaces the functions exported so they can't be confused with functions of the same name from the Haskell Prelude.

You will probably want to pay particular attention to the following functions exported by `Data.Map`:

```haskell
fromList :: Ord k => [(k, a)] -> Map k a
null :: Map k a -> Bool
lookup :: Ord k => k -> Map k a -> Maybe a
insert :: Ord k => k -> a -> Map k a -> Map k a
map :: (a -> b) -> Map k a -> Map k b
filter :: (a -> Bool) -> Map k a -> Map k a
keys :: Map k a -> [k]
intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
```