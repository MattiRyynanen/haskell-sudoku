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

loadPuzzle :: String -> [[Int]]
loadPuzzle = map loadCell where loadCell c = if c == '.' then [1..nine] else [(digitToInt c)]

withNo :: Eq a => a -> [a] -> [a]
withNo c = filter (/=c)

hasCandidate :: (Foldable t, Eq a) => a -> t a -> Bool
hasCandidate cand cell = elem cand cell

removeSingles :: Eq a => [[a]] -> [[a]]
removeSingles cells = applyWhileReduced removeSinglesStep cells

removeSinglesStep :: Eq a => [[a]] -> [[a]]
removeSinglesStep cells = removeSinglesStartingAt 0 cells

removeSinglesStartingAt :: Eq a => Int -> [[a]] -> [[a]]
removeSinglesStartingAt i cells
    | i >= length cells = cells
    | otherwise = removeSinglesStartingAt (succ i) (singlesRemovalAt i cells)

singlesRemovalAt :: Eq a => Int -> [[a]] -> [[a]]
singlesRemovalAt index cells
    | length (cells !! index) > 1 = cells
    | otherwise =
        let r = head (cells !! index)
            removalInds = [i | i <- intersectingEx index, hasCandidate r (cells !! i)]
        in [if elem i removalInds then withNo r c else c | (c, i) <- zip cells [0..]]

applyWhenIndex :: (Int -> Bool) -> (a -> a) -> [a] -> [a]
applyWhenIndex indexPred f xs = [if indexPred i then f x else x | (x, i) <- zip xs [0..]]

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt index f xs = applyWhenIndex (==index) f xs

onlyPossibilities :: [[Int]] -> [[Int]]
onlyPossibilities cells = applyWhileReduced (removeSingles . onlyPossibilitiesStartingAt 0) cells

applyWhileReduced :: Eq t => (t -> t) -> t -> t
applyWhileReduced f cells =
    let reduced = f cells
    in if reduced == cells then cells else applyWhileReduced f reduced

onlyPossibilitiesStartingAt i cells
    | i >= length cells = cells
    | otherwise = onlyPossibilitiesStartingAt (succ i) (onlyPossibilityAt i cells)

onlyPossibilityAt :: Int -> [[Int]] -> [[Int]]
onlyPossibilityAt index cells = onlyPossibilityAt' index (cells !! index) cells

onlyPossibilityAt' :: Int -> [Int] -> [[Int]] -> [[Int]]
onlyPossibilityAt' index cands cells
    | null cands = cells
    | any (==True) (map oneWithin indexSets) = applyAt index (\_ -> [cand]) cells
    | otherwise = onlyPossibilityAt' index (tail cands) cells
    where oneWithin indx = (countCandidates cand indx cells) == 1
          indexSets = map ($index) [rowIndicesAt, colIndicesAt, blockIndicesAt]
          cand = head cands

getAt :: [Int] -> [a] -> [a]
getAt = getAt' 0

getAt' :: Int -> [Int] -> [a] -> [a]
getAt' previousIndex indices items
    | null indices = []
    | null items = []
    | previousIndex > head indices = error $ concat ["Indices array needs to be in ascending order. ", show previousIndex, " > ", show (head indices)]
    | otherwise =
        let index = head indices
            remaining = drop (index - previousIndex) items
        in head remaining : getAt' index (tail indices) remaining

countCandidates :: Int -> [Int] -> [[Int]] -> Int
countCandidates cand indices cells = count (==cand) $ concat (getAt indices cells)

withColor :: Show a => a -> [Char] -> [Char]
withColor c str = concat ["\ESC[", show c, "m", str, "\ESC[0m"]

cellToStr :: Foldable t => [Char] -> t Int -> [Char]
cellToStr sep c = concat $ [if elem cand c then show cand else sep | cand <- [1..nine]]

showCell :: [Int] -> String
showCell c
    | length c == 1 = withColor 32 (cellToStr " " c)
    | length c == 2 = withColor 33 (cellToStr "." c)
    | otherwise = cellToStr "." c

showRow :: [[Int]] -> String
showRow = intercalate "|" . take9 . map showCell

showPuzzle :: [[Int]] -> String
showPuzzle cells
    | null cells = "\n"
    | otherwise = showRow cells ++ "\n" ++ (showPuzzle (drop nine cells))

printPuzzle :: [[Int]] -> IO ()
printPuzzle cells = putStr $ showPuzzle cells

p = loadPuzzle level5_hs_20200619
p1 = removeSingles p
p2 = onlyPossibilities p1

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
