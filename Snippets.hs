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
hasLength len = (==len) . length . take (succ len)

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