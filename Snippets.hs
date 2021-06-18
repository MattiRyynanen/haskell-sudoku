module Snippets (
    applyWhen,
    hasOne,
    hasTwo,
    without,
    elemDiff,
    group
)
where

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
