module Snippets where

import qualified Data.Set

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

filterWith :: [a -> Bool] -> [a] -> [a]
filterWith = filter . allConds
    where allConds ps c = all (\ p -> p c) ps

tripletCombinations :: [a] -> [[a]]
tripletCombinations xs = [[a, b, c]
    | (a, i) <- items, 
    (b, j) <- items,
    i < j,
    (c, k) <- items,
    j < k]
    where items = zip xs [0..]
