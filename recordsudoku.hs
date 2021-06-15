type Index = Int
type Candidate = Int
data Cell = Cell { index :: Index, candidates :: [Candidate] } deriving Show
type Puzzle = [Cell]

blockOf :: Cell -> Index
blockOf cell = 3 * (rowOf cell `div` 3) + colOf cell `div` 3

rowOf :: Cell -> Index
rowOf cell = index cell `div` 9

colOf :: Cell -> Index
colOf cell = index cell `rem` 9

posOf :: Cell -> String
posOf c = concat $ map (\f -> show $ f c) [rowOf, colOf, blockOf]

numCandidates c = length $ candidates c
isSolved c = numCandidates c == 1

intersectsEx :: Cell -> Cell -> Bool
intersectsEx a b = notSame && anyIntersection
    where ops = [rowOf, colOf, blockOf]
          notSame = index a /= index b
          anyIntersection = or $ zipWith (==) (map ($ a) ops) (map ($ b) ops)

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
removeCellCandidate cand cell = cell { candidates = withNo cand (candidates cell) }
setCellCandidate cand cell = cell { candidates = [cand] }

samePosThan a b = index a == index b

removeCandidate :: Puzzle -> Candidate -> Index -> Puzzle
removeCandidate puzzle cand ind
    | isSolved cell || hasNoCand cell cand = puzzle
    | numCandidates cell == 2 = setFinal puzzle cell cand
    | otherwise = applyWhen (samePosThan cell) (removeCellCandidate cand) puzzle
    where cell = puzzle !! ind

setFinal :: Puzzle -> Cell -> Candidate -> Puzzle
setFinal puzzle cell final
    | isSolved cell = error "Can't set final for a solved cell."
    | hasNoCand cell final = error "Can't set final since it is not in cell candidates."
    | otherwise = broadcastFinal withFinal
    where withFinal = applyWhen (samePosThan cell) (setCellCandidate final) puzzle
          intersectIndx = map (index) $ filter (intersectsEx cell) withFinal
          broadcastFinal puz = foldl (\p -> removeCandidate p final) puz intersectIndx

p = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]