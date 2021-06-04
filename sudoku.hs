import qualified Data.List as List
import qualified Data.Set as Set

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
