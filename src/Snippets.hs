module Snippets where

import Data.List (nub)

applyWhen :: (b -> Bool) -> (b -> b) -> [b] -> [b]
applyWhen p f = map (applyIf p f)

applyIf :: (p -> Bool) -> (p -> p) -> p -> p
applyIf p f x
    | p x = f x
    | otherwise = x

hasOne :: [a] -> Bool
hasOne [_] = True
hasOne _ = False

hasTwo :: [a] -> Bool
hasTwo [_, _] = True
hasTwo _ = False

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

countOccurences :: Eq a => [a] -> [(a, Int)]
countOccurences xs = [(e, count e) | e <- elems]
    where elems = nub xs
          count e = length $ filter (==e) xs
