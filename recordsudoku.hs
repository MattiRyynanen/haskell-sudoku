import Data.List (intercalate)
import qualified Data.Char
import qualified SamplePuzzles

type Index = Int
type Candidate = Int
data Cell = Cell { index :: Index, candidates :: [Candidate] } deriving (Show, Eq)
type Puzzle = [Cell]

rowAt = (`div` 9)
colAt = (`rem` 9)
blockAt ind = 3 * (rowAt ind `div` 3) + colAt ind `div` 3

blockOf :: Cell -> Index
blockOf = blockAt . index

rowOf :: Cell -> Index
rowOf = rowAt . index

colOf :: Cell -> Index
colOf = colAt . index

posOf :: Cell -> String
posOf c = concatMap (\f -> show $ f c) [rowOf, colOf, blockOf]

numCandidates :: Cell -> Int
numCandidates = length . candidates

isSolved :: Cell -> Bool
isSolved = (==1) . numCandidates

intersects :: Cell -> Cell -> Bool
intersects a b = or $ zipWith (==) (map ($ a) ops) (map ($ b) ops)
    where ops = [rowOf, colOf, blockOf]

intersectsEx :: Cell -> Cell -> Bool
intersectsEx a b = index a /= index b && intersects a b

hasCand :: Cell -> Candidate -> Bool
hasCand cell cand = cand `elem` candidates cell

hasNoCand :: Cell -> Candidate -> Bool
hasNoCand cell cand = not $ hasCand cell cand

tellCell :: Cell -> String
tellCell c = unwords [posOf c, show (candidates c)]

applyWhen pred f puzzle = [if pred c then f c else c | c <- puzzle]

withNo c = filter (/=c)
removeCellCandidate cand cell = cell { candidates = withNo cand (candidates cell) }
setCellCandidate cand cell = cell { candidates = [cand] }

samePosThan a b = index a == index b

removeCandidate :: Puzzle -> Candidate -> Index -> Puzzle
removeCandidate puzzle cand ind
    | isSolved cell || hasNoCand cell cand = puzzle
    | numCandidates cell == 2 = setFinal puzzle cell (head remaining)
    | otherwise = applyWhen (samePosThan cell) (removeCellCandidate cand) puzzle
    where cell = puzzle !! ind
          remaining = withNo cand $ candidates cell

hasLength len = (==len) . length . take (succ len)
hasOne = hasLength 1

solveOnlyPossibilityAt puzzle ind
    | isSolved cell = puzzle
    | length onlies == 1 = setFinal puzzle cell (head onlies)
    | length onlies > 1 = error $ unwords ["Found more than one possible final candidate: ", tellCell cell, showPuzzle puzzle]
    | otherwise = puzzle
    where onlies = searchOnlyPossibilityAt puzzle ind (candidates cell)
          cell = puzzle !! ind

searchOnlyPossibilityAt puzzle ind = filter (isOnlyPossibilityAt puzzle ind)

isOnlyPossibilityAt :: Puzzle -> Int -> Candidate -> Bool
isOnlyPossibilityAt puzzle ind cand
    | hasNoCand (puzzle !! ind) cand = error "Candidate not in the cell."
    | otherwise = any (onlyOneIn . get) [sameRow, sameCol, sameBlock]
    where sameRow = (== rowAt ind) . rowOf
          sameCol = (== colAt ind) . colOf
          sameBlock = (== blockAt ind) . blockOf
          get = flip filter puzzle
          onlyOneIn = hasOne . filter (==cand) . concatMap candidates

setFinal :: Puzzle -> Cell -> Candidate -> Puzzle
setFinal puzzle cell final
    | isSolved cell = error "Can't set final since it has solved already."
    | hasNoCand cell final = error "Can't set final since it is not in cell candidates."
    | otherwise = broadcastFinal withFinal
    where withFinal = applyWhen (samePosThan cell) (setCellCandidate final) puzzle
          intersectIndx = map index $ filter (intersectsEx cell) withFinal
          broadcastFinal puz = foldl (`removeCandidate` final) puz intersectIndx

countCandidates :: Candidate -> [Cell] -> Int
countCandidates cand = length . filter (`hasCand` cand)

createEmptyPuzzle :: Puzzle
createEmptyPuzzle = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]

loadPuzzle :: String -> Puzzle
loadPuzzle puzzle_str = foldl applyValue createEmptyPuzzle indx_values
    where indx_values = [(i, Data.Char.digitToInt s) | (i, s) <- zip [0..] puzzle_str, s /= '.']
          applyValue p iv = setFinal p (p !! fst iv) (snd iv)

pl = loadPuzzle SamplePuzzles.xtr_sud_04

withColor :: Show a => a -> [Char] -> [Char]
withColor c str = concat ["\ESC[", show c, "m", str, "\ESC[0m"]

showCell :: Cell -> String
showCell c
    | numCandidates c == 1 = withColor 32 (cellToStr " " c)
    | numCandidates c == 2 = withColor 33 (cellToStr "." c)
    | otherwise = cellToStr "." c
    where cellToStr sep c = concat $ [if hasCand c cand then show cand else sep | cand <- [1..9]]

group :: Int -> [a] -> [[a]]
group _ [] = []
group c xs = take c xs : group c (drop c xs)

noHighlights :: [Bool]
noHighlights = replicate (9 * 9) False

showPuzzle :: Puzzle -> String
showPuzzle cells = showPuzzleHighlights cells noHighlights

showPuzzleHighlights :: Puzzle -> [Bool] -> String
showPuzzleHighlights cells highlights = intercalate line (map concat (group 3 rows))
    where cellContents = [if h then withColor 44 (showCell c) else showCell c | (c, h) <- zip cells highlights]
          rows = map (('\n':) . intercalate "|") $ group 3 $ map unwords $ group 3 cellContents
          line = '\n' : intercalate "+" (replicate 3 (replicate 29 '-'))

printPuzzle :: Puzzle -> IO ()
printPuzzle cells = putStrLn $ showPuzzle cells

showSolution :: (Puzzle, String, [Bool]) -> String
showSolution sol = concat [reason, " Unsolved cells = ", show $ length $ filter (not . isSolved) puzzle, "\n", showPuzzleHighlights puzzle highlights, "\n"]
    where (puzzle, reason, highlights) = sol

showSolutions :: [(Puzzle, String, [Bool])] -> IO ()
showSolutions xs = mapM_ (putStrLn . showSolution) (reverse xs)

solutions :: [(Puzzle, String, [Bool])]
solutions = solve [(pl, "The loaded puzzle.", noHighlights)]

solve puzzles
    | all isSolved p = (p, "Solved!", noHighlights) : puzzles -- solved
    | p /= b = solve ((b, "Only possible candidate.", elemDiff p b) : puzzles)
--    | p /= bo = solve ((bo, "Omission: candidates in block on same row or column.", elemDiff p bo) : puzzles)
--    | p /= oib = solve ((oib, "Omission: candidates within one block.", elemDiff p oib) : puzzles )
--    | p /= c = solve ((c, "A naked pair.", elemDiff p c) : puzzles)
    | otherwise = (p, "No solution yet.", noHighlights) : puzzles -- no solution
    where (p, _, _) = head puzzles
          b = foldl solveOnlyPossibilityAt p [0..80]
          --c = solveNakedPair p
          --bo = solveBlockOmission p
          --oib = solveOmissionWithinBlock p

elemDiff :: Eq a => [a] -> [a] -> [Bool]
elemDiff xs ys = [x /= y | (x, y) <- zip xs ys]
