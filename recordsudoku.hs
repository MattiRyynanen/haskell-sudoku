import Data.List (intercalate)
import qualified Data.Char
import qualified SamplePuzzles
import Definitions
import Snippets
import Printers

intersects :: Cell -> Cell -> Bool
intersects a b = or $ zipWith (==) (map ($ a) ops) (map ($ b) ops)
    where ops = [rowOf, colOf, blockOf]

intersectsEx :: Cell -> Cell -> Bool
intersectsEx a b = index a /= index b && intersects a b

posOf :: Cell -> String
posOf c = concatMap (\f -> show $ f c) [rowOf, colOf, blockOf]

tellCell :: Cell -> String
tellCell c = unwords [posOf c, show (candidates c)]

samePosThan a b = index a == index b

removeCandidate :: Puzzle -> Candidate -> Index -> Puzzle
removeCandidate puzzle cand ind
    | isSolved cell || hasNoCand cand cell = puzzle
    | hasPair cell = setFinal puzzle cell (head remaining)
    | otherwise = applyWhen (samePosThan cell) (removeCellCandidate cand) puzzle
    where cell = puzzle !! ind
          remaining = without cand $ candidates cell

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
    | hasNoCand cand (puzzle !! ind) = error "Candidate not in the cell."
    | otherwise = any (onlyOneIn . get) [sameRow, sameCol, sameBlock]
    where sameRow = (== rowAt ind) . rowOf
          sameCol = (== colAt ind) . colOf
          sameBlock = (== blockAt ind) . blockOf
          get = flip filter puzzle
          onlyOneIn = hasOne . filter (==cand) . concatMap candidates

setFinal :: Puzzle -> Cell -> Candidate -> Puzzle
setFinal puzzle cell final
    | isSolved cell = error "Can't set final since it has solved already."
    | hasNoCand final cell = error "Can't set final since it is not in cell candidates."
    | otherwise = broadcastFinal withFinal
    where withFinal = applyWhen (samePosThan cell) (setCellCandidate final) puzzle
          intersectIndx = map index $ filter (intersectsEx cell) withFinal
          broadcastFinal puz = foldl (`removeCandidate` final) puz intersectIndx

createEmptyPuzzle :: Puzzle
createEmptyPuzzle = [Cell {index = i, candidates=[1..9]} | i <- [0..80]]

loadPuzzle :: String -> Puzzle
loadPuzzle puzzle_str = foldl applyValue createEmptyPuzzle indx_values
    where indx_values = [(i, Data.Char.digitToInt s) | (i, s) <- zip [0..] puzzle_str, s /= '.']
          applyValue p iv = setFinal p (p !! fst iv) (snd iv)

pl = loadPuzzle SamplePuzzles.xtr_sud_04

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
