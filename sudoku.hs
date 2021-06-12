import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Char (digitToInt)

nine = 9
three = 3
numCells = nine * nine
allIndices = [0..pred numCells]

take9 :: [a] -> [a]
take9 = take nine

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

rowIndices :: Int -> [Int]
rowIndices r = take9 [nine * r..]

colIndices :: Int -> [Int]
colIndices c = take9 [c, (c + nine)..]

blockIndices :: Int -> [Int]
blockIndices c = take9 [i | i <- allIndices, blockOf i == c]

cellSetIndices :: [[Int]]
cellSetIndices = concat $ map cellSetIndicesAt [0..8]

cellSetIndicesAt :: Int -> [[Int]]
cellSetIndicesAt id
    | id >= nine = error ("Set index needs to be in range 0..8, got " ++ show id)
    | otherwise = map ($id) [rowIndices, colIndices, blockIndices]

rowOf :: Int -> Int
rowOf ind = div ind nine

colOf :: Int -> Int
colOf ind = rem ind nine

blockOf :: Int -> Int
blockOf ind =
    let r = div (rowOf ind) three
        c = div (colOf ind) three
    in three * r + c

colIndicesAt :: Int -> [Int]
colIndicesAt = colIndices . colOf

rowIndicesAt :: Int -> [Int]
rowIndicesAt = rowIndices . rowOf

blockIndicesAt :: Int -> [Int]
blockIndicesAt = blockIndices . blockOf

intersecting :: Int -> [Int]
intersecting ind = concat $ map ($ind) [rowIndicesAt, colIndicesAt, blockIndicesAt]

intersectingEx :: Int -> [Int]
intersectingEx ind = withNo ind $ intersecting ind

applyWhenIndex :: (Int -> Bool) -> (a -> a) -> [a] -> [a]
applyWhenIndex pred f xs = [if pred i then f x else x | (x, i) <- zip xs [0..]]

setAt :: Int -> a -> [a] -> [a]
setAt index to = applyWhenIndex (==index) (\_ -> to)

applyWhileReduced :: Eq t => (t -> t) -> t -> t
applyWhileReduced f cells =
    let reduced = f cells
    in if reduced == cells then cells else applyWhileReduced f reduced

withNo :: Eq a => a -> [a] -> [a]
withNo c = filter (/=c)

loadPuzzle :: String -> [[Int]]
loadPuzzle = map loadCell where loadCell c = if c == '.' then [1..nine] else [(digitToInt c)]

singlesRemovalAt :: Eq a => [[a]] -> Int -> [[a]]
singlesRemovalAt cells index
    | length cell > 1 = cells -- no single candidate
    | otherwise = applyWhenIndex (\i -> elem i intersectingIndices) removal cells
    where cell = cells !! index
          removal = withNo $ head cell
          intersectingIndices = intersectingEx index

removeSingles :: Eq a => [[a]] -> [[a]]
removeSingles cells = applyWhileReduced removeSinglesOnce cells

removeSinglesOnce :: Eq a => [[a]] -> [[a]]
removeSinglesOnce cells = foldl singlesRemovalAt cells allIndices

solveOnlyPossibilities :: [[Int]] -> [[Int]]
solveOnlyPossibilities cells = foldl onlyPossibilityAt cells allIndices

onlyPossibilityAt :: [[Int]] -> Int -> [[Int]]
onlyPossibilityAt cells index = onlyPossibilityAt' index (cells !! index) cells

notEmpty xs = not $ null xs

onlyPossibilityAt' :: Int -> [Int] -> [[Int]] -> [[Int]]
onlyPossibilityAt' index cands cells
    | null cands = cells
    | any (==True) (map exactlyOneIn indices) = setAt index [cand] cells
    | otherwise = onlyPossibilityAt' index (tail cands) cells
    where exactlyOneIn indx = (countCandidates cand indx cells) == 1
          indices = map ($index) [rowIndicesAt, colIndicesAt, blockIndicesAt]
          cand = head cands

solveNakedPair cells
    | null nps = cells -- no naked pairs removal
    | otherwise = applyWhenIndex (\i -> elem i indx) (\cell -> withNoAny pair cell) cells
    where nps = findNakedPairRemovals cells
          (pair, indx) = head nps

findNakedPairRemovals cells = concat $ filter notEmpty [findNakedPairRemovals' indx cells | indx <- cellSetIndices]

findNakedPairRemovals' indices cells = [(pair, indx) | (pair, indx) <- zip pairs removing_pairs, notEmpty indx]
    where pairs = possiblePairs $ getAt indices cells
          removing_pairs = map (nakedPairRemoves indices cells) pairs

-- nakedPairRemoves (rowIndices 4) px [4,6]
-- Find the indices of cells where given pair eliminates candidates.
nakedPairRemoves :: [Int] -> [[Int]] -> [Int] -> [Int]
nakedPairRemoves indices cells pair
    | length pair_inds == 2 = affected_inds
    | otherwise = []
    where cs = zip (getAt indices cells) indices
          pair_inds = [i | (c, i) <- cs, c == pair]
          affected_inds = [i | (c, i) <- cs, removalApplies c]
          removalApplies cell = length cell > 1 && cell /= pair && length (withNoAny pair cell) > 0

withNoAny :: [Int] -> [Int] -> [Int]
withNoAny cands cell = [c | c <- cell, not (elem c cands)]

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

possiblePairs :: [[Int]] -> [[Int]]
possiblePairs = unique . filter (\c -> length c == 2)

getAt :: [Int] -> [a] -> [a]
getAt = getAt' 0

getAt' :: Int -> [Int] -> [a] -> [a]
getAt' previousIndex indices items
    | null indices || null items = []
    | previousIndex > head indices = error $ concat ["Indices array needs to be in ascending order. ", show previousIndex, " > ", show (head indices)]
    | otherwise =
        let index = head indices
            remaining = drop (index - previousIndex) items
        in head remaining : getAt' index (tail indices) remaining

countCandidates :: Int -> [Int] -> [[Int]] -> Int
countCandidates cand indices = count (==cand) . concat . getAt indices

withColor :: Show a => a -> [Char] -> [Char]
withColor c str = concat ["\ESC[", show c, "m", str, "\ESC[0m"]

showCell :: [Int] -> String
showCell c
    | length c == 1 = withColor 32 (cellToStr " " c)
    | length c == 2 = withColor 33 (cellToStr "." c)
    | otherwise = cellToStr "." c
    where cellToStr sep c = concat $ [if elem cand c then show cand else sep | cand <- [1..nine]]

showSolution :: ([[Int]], String, [Bool]) -> String
showSolution sol = concat [reason, " Number of candidates = ", show $ length $ concat cells, "\n", (showPuzzleHighlights cells highlights), "\n"]
    where (cells, reason, highlights) = sol

group :: Int -> [a] -> [[a]]
group _ [] = []
group c xs = take c xs : group c (drop c xs)

noHighlights :: [Bool]
noHighlights = replicate numCells False

showPuzzle :: [[Int]] -> String
showPuzzle cells = showPuzzleHighlights cells noHighlights

showPuzzleHighlights :: [[Int]] -> [Bool] -> String
showPuzzleHighlights cells highlights = intercalate line (map concat (group three rows))
    where cellContents = [if h then withColor 44 (showCell c) else showCell c | (c, h) <- zip cells highlights]
          rows = map ('\n':) $ map (intercalate "|") $ group nine cellContents
          line = '\n' : replicate 89 '-'

showSolutions xs = mapM_ (putStrLn . showSolution) (reverse xs)

printPuzzle :: [[Int]] -> IO ()
printPuzzle cells = putStrLn $ showPuzzle cells

p = loadPuzzle xtr_sud_04 --level5_hs_20200619
(px, _, _) = head solutions
px_pairs = getAt (rowIndices 4) px

solutions = solve [(p, "The loaded puzzle.", noHighlights)]

solve puzzles
    | (length $ concat p) == numCells = (p, "Solved!", noHighlights) : puzzles -- solved
    | p /= a = solve ((a, "Singles removed.", elemDiff p a) : puzzles)
    | p /= b = solve ((b, "Only possible candidate.", elemDiff p b) : puzzles)
    | p /= c = solve ((c, "A naked pair.", elemDiff p c) : puzzles)
    | otherwise = (p, "No solution yet.", noHighlights) : puzzles -- no solution
    where (p, _, _) = head puzzles
          a = removeSingles p
          b = solveOnlyPossibilities p
          c = solveNakedPair p

elemDiff xs ys = [x /= y | (x, y) <- zip xs ys]

level5_hs_20200619 = 
  "2......91" ++
  ".46..2..." ++
  "..1..7.3." ++
  ".....9.7." ++
  ".95...28." ++
  ".1.8....." ++
  ".5.1..9.." ++
  "...7..52." ++
  "42......7"

xtr_sud_04 =
    "..4...3.." ++
    "..7...2.." ++
    "8..2.5..4" ++
    "..58.69.." ++
    "4..9.2..6" ++
    "....4...." ++
    "...6.4..." ++
    "1..3.7..5" ++
    ".2..1..9."