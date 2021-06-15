import qualified Data.Set as Set

--Set.fromList [1..9]

type Index = Int
type Candidate = Int
data Cell = Cell { index :: Index, candidates :: [Candidate] } deriving Show
type Puzzle = [Cell]

a = Cell { index = 3, candidates=[1,2] }

blockOf :: Cell -> Index
blockOf cell = 3 * (rowOf cell `div` 3) + colOf cell `div` 3

rowOf :: Cell -> Index
rowOf cell = index cell `div` 9

colOf :: Cell -> Index
colOf cell = index cell `rem` 9

posOf :: Cell -> String
posOf c = concat $ map (\f -> show $ f c) [rowOf, colOf, blockOf]

hasCand :: Cell -> Candidate -> Bool
hasCand cell cand = elem cand (candidates cell)

hasNoCand :: Cell -> Candidate -> Bool
hasNoCand cell cand = not $ hasCand cell cand

tellCell :: Cell -> String
tellCell c = concat $ map (show) [rowOf c, colOf c, blockOf c]

applyWhen pred f puzzle = [if pred c then f c else c | c <- puzzle]

applyAt :: Index -> (Cell -> Cell) -> Puzzle -> Puzzle
applyAt i f puzzle = applyWhen (\c -> index c == i) f puzzle

withNo c = filter (/=c)
cellAt puzzle i = head $ filter (\c -> index c == i) puzzle
removeCellCandidate cell cand = cell { candidates = withNo cand (candidates cell) }
setCellCandidate cell cand = cell { candidates = [cand] }

removeCandidate :: Puzzle -> Index -> Candidate -> Puzzle
removeCandidate puzzle index cand
    | otherwise = puzzle
    where cell = cellAt puzzle index

setValue :: Puzzle -> Index -> Candidate -> Puzzle
setValue puzzle index cand = newPuzzle
    where newPuzzle = applyAt index (\c -> setCellCandidate c cand) puzzle

p = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]