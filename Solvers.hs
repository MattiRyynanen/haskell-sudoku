module Solvers where

import Definitions
import Snippets
import Data.Maybe
import Data.List (find)

data Solver = Solver { transformer :: Puzzle -> Puzzle, name :: String, level :: Int }
instance Show Solver where show s = concat [show $ level s, ": ", name s]

data SolutionStep = SolutionStep { result :: Puzzle, previous :: Puzzle, solver :: Solver } deriving (Show)

applyWhileReduced f puzzle =
    let reduced = f puzzle
    in if reduced == puzzle then puzzle else applyWhileReduced f reduced

solvers = let s = Solver in [
    s (applyWhileReduced removeSolved) "Removed candidates by solved values." 0,
    s solveOnlyPossibility "Only possible candidate." 0,
    s solveBlockOmissions "Omission: candidates in block on same row or column." 1,
    s solveOmitCandidateInOneBlock "Omission: candidates within one block." 1,
    s solveNakedPairs "Naked pairs." 2,
    s solveHiddenPair "Hidden pairs." 2,
    s solveNakedTriplets "Naked triplets." 3,
    s solveHiddenTriplet "Hidden triplets." 4,
    s solveXwing "X-Wing." 5]

idSolver n = Solver id n 100
idleStep puzzle id = SolutionStep puzzle puzzle (idSolver id)

solve :: [SolutionStep] -> [SolutionStep]
solve steps
    | null steps = error "Nothing to solve."
    | all isSolved latest = prepend $ idleStep latest "Solved!"
    | isJust simplestSolver = solve $ prepend $ stepFor simplestSolver
    | otherwise = prepend $ idleStep latest "No solution yet."
    where latest = result $ head steps -- the latest puzzle
          simplestSolver = find (\solver -> transformer solver latest /= latest) solvers
          prepend step = step : steps
          stepFor solver = let s = fromJust solver in SolutionStep (transformer s latest) latest s

-- Remove candidates based on already solved cells.

removeSolved :: Puzzle -> Puzzle
removeSolved puzzle = map removeCandidates puzzle
    where solved = filter isSolved puzzle
          candidatesToRemoveFor cell = concatMap candidates $ filter (intersectsEx cell) solved
          removeCandidates cell = foldl (flip removeCellCandidate) cell (candidatesToRemoveFor cell)

-- Only possibility solver:

solveOnlyPossibility :: Puzzle -> Puzzle
solveOnlyPossibility puzzle = foldl solveOnlyPossibilityAt puzzle [0..80]

solveOnlyPossibilityAt :: Puzzle -> Index -> Puzzle
solveOnlyPossibilityAt puzzle ind
    | isSolved cell = puzzle
    | length onlies == 1 = setSolvedAt (head onlies) ind puzzle
    | length onlies > 1 = error $ unwords ["Found more than one possible final candidate: ", show cell]
    | otherwise = puzzle
    where onlies = searchOnlyPossibilityAt puzzle ind (candidates cell)
          cell = puzzle !! ind

searchOnlyPossibilityAt :: Puzzle -> Int -> [Candidate] -> [Candidate]
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

-- Block omission, candidates within one block on one row or column, only.
-- Can remove the possible candidates on that row or column on adjacent block.

applyRemover :: Puzzle -> (Puzzle -> Puzzle) -> Puzzle
applyRemover puzzle remover = remover puzzle

solveBlockOmissions :: Puzzle -> Puzzle
solveBlockOmissions puzzle = foldl applyRemover puzzle (rowRemovers ++ colRemovers)
    where rowRemovers = concatMap (searchBlockOmissionBy rowOf puzzle) [0..8]
          colRemovers = concatMap (searchBlockOmissionBy colOf puzzle) [0..8]

searchBlockOmissionBy :: (Cell -> Index) -> Puzzle -> Index -> [Puzzle -> Puzzle]
searchBlockOmissionBy indexer puzzle blockIndex = removers
    where unsolved = filterWith [(==blockIndex) . blockOf, isUnsolved] puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint indexer (withCandidate cand unsolved)
          removerFor ind cand = applyWhen (\c -> indexer c == ind && blockOf c /= blockIndex) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

-- Block omission: row or column has a candidate only within one block, any other
-- candidate in that block can be removed.

solveOmitCandidateInOneBlock :: Puzzle -> Puzzle
solveOmitCandidateInOneBlock puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchOmitCandidateInOneBlock puzzle) (concatMap selectors [rowOf, colOf])
          selectors f = map (\i -> (==i) . f) [0..8]

searchOmitCandidateInOneBlock :: Puzzle -> (Cell -> Bool) -> [Puzzle -> Puzzle]
searchOmitCandidateInOneBlock puzzle p = removers
    where unsolved = filterWith [p, isUnsolved] puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint blockOf (withCandidate cand unsolved)
          removerFor blockInd cand = applyWhen (\c -> not (p c) && blockOf c == blockInd) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

-- Solving Naked pair
-- If row, column, or block contains two pairs with exactly same candidates,
-- the numbers in elsewhere within the house can be removed.

houseSelectors :: [Cell -> Bool]
houseSelectors = [(==i) . f | f <- [rowOf, colOf, blockOf], i <- [0..8]]

solveNakedPairs :: Puzzle -> Puzzle
solveNakedPairs puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchNakedPair puzzle) houseSelectors

searchNakedPair :: [Cell] -> (Cell -> Bool) -> [Puzzle -> Puzzle]
searchNakedPair puzzle p = map removerFor $ filterWith [not . null, hasRemovals, isNakedPair] unique_pairs
    where unsolved = filterWith [p, isUnsolved] puzzle
          pair_cells = filter hasPair unsolved
          unique_pairs = unique $ map candidates pair_cells
          isNakedPair pair = hasTwo $ filter (==pair) $ map candidates pair_cells
          hasRemovals pair = any (\cell -> hasAnyCand pair cell && candidates cell /= pair) unsolved
          removerFor pair = applyWhen (\c -> p c && candidates c /= pair) (removeCellCandidates pair)

-- *Main> searchHiddenPair px ((==7) . rowOf)
-- [(2,[65,67]),(8,[65,67])]

solveHiddenPair :: Puzzle -> Puzzle
solveHiddenPair puzzle = foldl applyRemover puzzle removers
    where ps = concatMap (searchHiddenPair puzzle) houseSelectors
          removerFor p = applyWhen (\c -> index c `elem` fst p) (setCellCandidates $ snd p)
          removers = map removerFor ps

searchHiddenPair :: Puzzle -> (Cell -> Bool) -> [([Index], [Candidate])]
searchHiddenPair puzzle p = posCands
    where unsolved = filterWith [p, isUnsolved] puzzle
          unique_cands = unique $ concatMap candidates unsolved
          positionsFor cand = map index $ filter (hasCand cand) unsolved
          twoPosCands = [(positionsFor cand, cand) | cand <- unique_cands, hasTwo $ positionsFor cand]
          unique_positions = unique $ map fst twoPosCands
          candsForP p = map snd $ filter ((==p) . fst) twoPosCands
          posCands = [(p, candsForP p) | p <- unique_positions, hasTwo $ candsForP p]

solveNakedTriplets :: Puzzle -> Puzzle
solveNakedTriplets puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchNakedTriplets puzzle) houseSelectors

searchNakedTriplets :: Puzzle -> (Cell -> Bool) -> [Puzzle -> Puzzle]
searchNakedTriplets puzzle p
    | length unsolved <= 3 = []
    | otherwise = map removerFor validCombs
    where unsolved = filterWith [p, isUnsolved] puzzle
          combinations = tripletCombinations unsolved
          hasThreeCandidates comb = (==3) $ length $ candidatesIn comb
          candidatesIn comb = unique $ concatMap candidates comb
          validCombs = filter hasThreeCandidates combinations
          indicesFor comb = map index comb
          removerFor comb = applyWhen (\c -> p c && index c `notElem` indicesFor comb) (removeCellCandidates (candidatesIn comb))

solveHiddenTriplet puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchHiddenTriplet puzzle) houseSelectors

searchHiddenTriplet :: Puzzle -> (Cell -> Bool) -> [Puzzle -> Puzzle]
searchHiddenTriplet puzzle p
    | length unsolved <= 3 = []
    | otherwise = map removerFor validCombs
    where unsolved = filterWith [p, isUnsolved] puzzle
          unique_cands = unique $ concatMap candidates unsolved
          positionsFor cand = map index $ filter (hasCand cand) unsolved
          tripletPosCands = [(positionsFor cand, cand) | cand <- unique_cands, (<=3) $ length $ positionsFor cand]
          combs = tripletCombinations tripletPosCands
          combIndx comb = unique $ concatMap fst comb
          validCombs = filter ((==3) . length . combIndx) combs
          indicesFor comb = unique $ concatMap fst comb
          candidatesIn comb = unique $ map snd comb
          removerFor comb = applyWhen (\c -> p c && index c `elem` indicesFor comb) (keepOnlyCandidates (candidatesIn comb))

solveXwing :: Puzzle -> Puzzle
solveXwing puzzle = foldl applyRemover puzzle removers
    where removers = concat [searchXwing puzzle sel cand | cand  <- [1..9], sel <- ["rows", "cols"]]

searchXwing :: Puzzle -> String -> Candidate -> [Puzzle -> Puzzle]
searchXwing puzzle selId cand
    | length unsolved <= 4 = []
--    | otherwise = concatMap removalCells validCombs -- for testing
    | otherwise = map removerFor (concatMap removalCells validCombs)
    where unsolved = filterWith [hasCand cand, isUnsolved] puzzle
          ops = if selId == "rows" then (rowOf, colOf) else (colOf, rowOf)
          selector i = map (snd ops) $ filter ((==i) . fst ops) unsolved
          twoPos f = [(i, f i) | i <- [0..8], hasTwo $ f i]
          validComb comb = allSame $ map snd comb
          validCombs = map (\comb -> (map fst comb, head $ map snd comb)) $ filter validComb (pairCombinations $ twoPos selector)
          removalCells comb = filter (\c -> snd ops c `elem` snd comb && fst ops c `notElem` fst comb && hasCand cand c) unsolved
          removerFor cell = applyWhen (samePosWith cell) (removeCellCandidate cand)
