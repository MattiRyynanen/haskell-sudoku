module Hudoku.Snippets where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable

applyWhen :: (b -> Bool) -> (b -> b) -> [b] -> [b]
applyWhen p f = map (applyIf p f)

applyIf :: (p -> Bool) -> (p -> p) -> p -> p
applyIf p f x
    | p x = f x
    | otherwise = x

{- |
Returns true if list has one element, otherwise false.

>>> map hasOne [[], [1], [1,2]]
[False,True,False]
-}

hasOne :: [a] -> Bool
hasOne [_] = True
hasOne _ = False

{- |
Returns true if list has two elements, otherwise false.

>>> map hasTwo [[], [1], [1,2], [1,2,3]]
[False,False,True,False]
-}

hasTwo :: [a] -> Bool
hasTwo [_, _] = True
hasTwo _ = False

{- | Returns True if elem in is ordered list.

>>> elemInOrdered 7 [1,3,7,7]
True

>>> elemInOrdered 7 [6,9,12]
False

>>> elemInOrdered 7 []
False
-}
elemInOrdered :: Ord a => a -> [a] -> Bool
elemInOrdered x = check
    where check [] = False
          check (y:ys) = case x `compare` y of
                             LT -> False
                             EQ -> True
                             GT -> check ys

group :: Int -> [a] -> [[a]]
group _ [] = []
group c xs = take c xs : group c (drop c xs)

unique :: Eq a => [a] -> [a]
unique = nub

allSame :: Eq a => [a] -> Bool
allSame [] = error "Can't check for empty list."
allSame xs = all (== head xs) $ tail xs

filterWith :: Foldable t => t (a -> Bool) -> [a] -> [a]
filterWith fs xs = foldl (flip filter) xs fs

{- |
Creates list of unique pair combinations.

>>> pairCombinations [1,2,3,4]
[[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
-}

pairCombinations :: [a] -> [[a]]
pairCombinations xs = [[a, b] | (a, i) <- xsi, (b, j) <- xsi, i < j]
    where xsi = zip xs [0 :: Int ..]

tripletCombinations :: [a] -> [[a]]
tripletCombinations xs = [[a, b, c]
    | (a, i) <- items,
    (b, j) <- items,
    i < j,
    (c, k) <- items,
    j < k]
    where items = zip xs [0 :: Int ..]

{- | Create a map with unique elements as keys and their counts as values.

>>> countOccurrences "baaddcccdd"
fromList [('a',2),('b',1),('c',3),('d',4)]
-}
countOccurrences :: Foldable t => Ord a => t a -> Map.Map a Int
countOccurrences = foldl (flip addOne) Map.empty
    where addOne item = Map.insertWith (+) item 1

{- | Update element at given index.

>>> updateAt 9 (*10) [0..9]
[0,1,2,3,4,5,6,7,8,90]
-}
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = toList $ Seq.adjust' f i (Seq.fromList xs)
