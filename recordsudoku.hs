import Data.List (intercalate)
import qualified Data.Char
import qualified SamplePuzzles

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
intersectsEx a b = notSamePos && anyIntersection
    where ops = [rowOf, colOf, blockOf]
          notSamePos = index a /= index b
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
    | numCandidates cell == 2 = setFinal puzzle cell final_value
    | otherwise = applyWhen (samePosThan cell) (removeCellCandidate cand) puzzle
    where cell = puzzle !! ind
          final_value = head $ withNo cand $ candidates cell

setFinal :: Puzzle -> Cell -> Candidate -> Puzzle
setFinal puzzle cell final
    | isSolved cell = error "Can't set final for a solved cell."
    | hasNoCand cell final = error "Can't set final since it is not in cell candidates."
    | otherwise = broadcastFinal withFinal
    where withFinal = applyWhen (samePosThan cell) (setCellCandidate final) puzzle
          intersectIndx = map (index) $ filter (intersectsEx cell) withFinal
          broadcastFinal puz = foldl (\p ind -> removeCandidate p final ind) puz intersectIndx

createEmptyPuzzle :: Puzzle
createEmptyPuzzle = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]

loadPuzzle :: String -> Puzzle
loadPuzzle puzzle_str = foldl applyValue createEmptyPuzzle indx_values
    where indx_values = [(i, Data.Char.digitToInt(s)) | (i, s) <- zip [0..] puzzle_str, s /= '.']
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
          rows = map ('\n':) $ map (intercalate "|") $ group 3 $ map (intercalate " ") $ group 3 cellContents
          line = '\n' : (intercalate "+" $ replicate 3 (replicate 29 '-'))

printPuzzle :: Puzzle -> IO ()
printPuzzle cells = putStrLn $ showPuzzle cells