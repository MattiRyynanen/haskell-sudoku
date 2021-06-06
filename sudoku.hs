import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Char (digitToInt)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative or zero n"

s = group 9 [0..80]

sudokuSize = 9
blocksLen = 3
allIndices = [0..pred (sudokuSize * sudokuSize)]

sudokuNumber :: Int -> Int
sudokuNumber c 
    | (c >= 1 && c <= sudokuSize) = c
    | otherwise = error $ "Sudoku number needs to be in range [1, " ++ show sudokuSize ++ "]"

take9 :: [a] -> [a]
take9 = take sudokuSize

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

countEq :: Eq a => a -> [a] -> Int
countEq n = count (==n)

rowIndices :: Int -> [Int]
rowIndices r = take9 [(sudokuSize * r)..]

colIndices :: Int -> [Int]
colIndices c = take9 [c, (c + sudokuSize)..]

blockIndex :: Int -> Int
blockIndex n = div n blocksLen

rowOf :: Int -> Int
rowOf ind = head $ from1dInd9 ind

colOf :: Int -> Int
colOf ind = last $ from1dInd9 ind

blockOf :: Int -> Int
blockOf ind = 
    let b_rc = map blockIndex (from1dInd9 ind)
    in to1dInd blocksLen (head b_rc) (last b_rc)

blockIndices :: Int -> [Int]
blockIndices c = take9 [i | i <- allIndices, blockOf i == c]

intersecting :: Int -> [Int]
intersecting ind = (rowIndices $ rowOf ind) ++ (colIndices $ colOf ind) ++ (blockIndices $ blockOf ind)

intersectingEx :: Int -> [Int]
intersectingEx ind = filter (/=ind) $ intersecting ind

to1dInd :: Int -> Int -> Int -> Int
to1dInd s r c = s * r + c

from1dInd :: Integral b => b -> b -> [b]
from1dInd s i = [div i s, rem i s]

from1dInd9 :: Int -> [Int]
from1dInd9 = from1dInd sudokuSize

loadCell :: Char -> [Int]
loadCell c
    | c == '.' = [1..9]
    | otherwise = [(digitToInt c)]

loadPuzzle :: String -> [[Int]]
loadPuzzle p = map loadCell p

withNo :: Eq a => a -> [a] -> [a]
withNo c xs = filter (/=c) xs

hasCandidate :: (Foldable t, Eq a) => a -> t a -> Bool
hasCandidate cand cell = elem cand cell

removeSingles :: Eq a => [[a]] -> [[a]]
removeSingles cells =
    let rc = removeSinglesStep cells
    in if rc == cells then cells else removeSingles rc

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
            newCells = [if elem i removalInds then withNo r c else c | (c, i) <- zip cells [0..]]
        in newCells

onlyPossibleAt index cand cells =
    let rs = withNo index (rowIndices $ rowOf index)
        cell = cells !! index
        rc = [cells !! i | i <- rs]
    in [c | c <- rc, hasCandidate cand c]

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


onlyOnCol index cand cells =
    let cell = cells !! index
    in count (==cand) $ concat [cells !! i | i <- colIndices (colOf index)]

withColor :: Show a => a -> [Char] -> [Char]
withColor c str = "\ESC[" ++ show c ++ "m" ++ str ++ "\ESC[0m"

cellToStr :: Foldable t => [Char] -> t Int -> [Char]
cellToStr sep c = concat $ [if elem cand c then show cand else sep | cand <- [1..sudokuSize]]

showCell :: [Int] -> String
showCell c
    | length c == 1 = withColor 32 (cellToStr " " c)
    | length c == 2 = withColor 33 (cellToStr "." c)
    | otherwise = cellToStr "." c

showRow :: [[Int]] -> String
showRow cells = intercalate " " (take9 $ map showCell cells)

showPuzzle :: [[Int]] -> String
showPuzzle cells
    | null cells = "\n"
    | otherwise = showRow cells ++ "\n" ++ (showPuzzle (drop 9 cells))

printPuzzle :: [[Int]] -> IO ()
printPuzzle cells = putStr $ showPuzzle cells

p = loadPuzzle level5_hs_20200619
p1 = removeSingles p

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

