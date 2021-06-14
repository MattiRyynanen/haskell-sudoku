import qualified Data.Set as Set

--Set.fromList [1..9]

type Index = Int
type Candidate = Int
data Cell = Cell { index :: Index, candidates :: [Candidate] } deriving Show

a = Cell { index = 3, candidates=[1,2] }

blockOf :: Cell -> Index
blockOf cell = 3 * (rowOf cell `div` 3) + colOf cell `div` 3

rowOf :: Cell -> Index
rowOf cell = index cell `div` 9

colOf :: Cell -> Index
colOf cell = index cell `rem` 9

posOf :: Cell -> String
posOf c = concat $ map (\f -> show $ f c) [rowOf, colOf, blockOf]

tellCell :: Cell -> String
tellCell c = concat $ map (show) [rowOf c, colOf c, blockOf c]

puzzle = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]