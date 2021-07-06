module Snippets where

import qualified Data.Set
import Data.List (nub)

applyWhen :: (b -> Bool) -> (b -> b) -> [b] -> [b]
applyWhen pred func = map (applyIf pred func)

applyIf :: (p -> Bool) -> (p -> p) -> p -> p
applyIf pred func x
    | pred x = func x
    | otherwise = x

hasOne :: [a] -> Bool
hasOne [_] = True
hasOne _ = False

hasTwo :: [a] -> Bool
hasTwo [_, _] = True
hasTwo _ = False

hasLength :: Int -> [a] -> Bool
hasLength 0 xs = null xs
hasLength len xs = hasOne $ dropWhile (<len) $ zipWith const [1 ..] xs

without :: Eq a => a -> [a] -> [a]
without x = filter (/=x)

elemDiff :: Eq a => [a] -> [a] -> [Bool]
elemDiff = zipWith (/=)

group :: Int -> [a] -> [[a]]
group _ [] = []
group c xs = take c xs : group c (drop c xs)

unique :: Ord a => [a] -> [a]
unique = Data.Set.toList . Data.Set.fromList

allSame :: Eq a => [a] -> Bool
allSame [] = error "Can't check for empty list."
allSame xs = all (== head xs) $ tail xs

filterWith :: Foldable t => t (a -> Bool) -> [a] -> [a]
filterWith fs xs = foldl (flip filter) xs fs

pairCombinations :: [a] -> [[a]]
pairCombinations xs = [[a, b] | (a, i) <- xsi, (b, j) <- xsi, i < j]
    where xsi = zip xs [0..]

tripletCombinations :: [a] -> [[a]]
tripletCombinations xs = [[a, b, c]
    | (a, i) <- items, 
    (b, j) <- items,
    i < j,
    (c, k) <- items,
    j < k]
    where items = zip xs [0..]

countOccurences :: Eq a => [a] -> [(a, Int)]
countOccurences xs = [(e, count e) | e <- elems]
    where elems = nub xs
          count e = length $ filter (==e) xs
