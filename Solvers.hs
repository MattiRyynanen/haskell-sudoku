module Solvers where

import Definitions
import Snippets
import Data.Maybe
import Data.List (find)

type Transformer = Puzzle -> Puzzle

data Solver = Solver { transformer :: Transformer, name :: String, level :: Int }
instance Show Solver where show s = concat [show $ level s, ": ", name s]

data IdleStepId = Initial | Solved | NoSolution | InvalidSolution deriving (Eq, Show)

data SolutionStep =
    IdleStep { puzzle :: Puzzle, step :: IdleStepId } | 
    SolverStep { result :: Puzzle, previous :: Puzzle, solver :: Solver } deriving (Show)

getPuzzle :: SolutionStep -> Puzzle
getPuzzle IdleStep {puzzle = p}  = p
getPuzzle SolverStep {result = p} = p

applyWhileReduced f puzzle =
    let reduced = f puzzle
    in if reduced == puzzle then puzzle else applyWhileReduced f reduced

solvers = let s = Solver in [
    s (applyWhileReduced removeSolved) "Removed candidates by solved values." 0,
    s solveSingles "Singles, the only possibilities in any house." 0,
    s solveBlockOmissions "Omission: candidates in block on same row or column." 1,
    s solveOmitCandidateInOneBlock "Omission: candidates within one block." 1,
    s solveNakedPairs "Naked pairs." 2,
    s solveHiddenPair "Hidden pairs." 2,
    s solveNakedTriplets "Naked triplets." 3,
    s solveHiddenTriplet "Hidden triplets." 4,
    s solveXwing "X-Wing." 5,
    s solveUniqueRectangle "Unique rectangle." 5,
    s solveXyWing "XY-Wing." 6]

--idSolver n = Solver id n 100
--idleStep puzzle id = SolutionStep puzzle puzzle (idSolver id)

solve :: [SolutionStep] -> [SolutionStep]
solve steps
    | null steps = error "Nothing to solve."
    | not $ housesOk latest = addIdleStep InvalidSolution
    | hasZeroCandidates latest = addIdleStep InvalidSolution
    | all isSolved latest = addIdleStep Solved
    | isJust simplestSolver = solve $ prepend $ stepFor simplestSolver
    | otherwise = addIdleStep NoSolution
    where latest = getPuzzle $ head steps -- the latest puzzle
          simplestSolver = find (\solver -> transformer solver latest /= latest) solvers
          prepend step = step : steps
          stepFor solver = let s = fromJust solver in SolverStep (transformer s latest) latest s
          addIdleStep stepId = prepend $ IdleStep latest stepId

-- Remove candidates based on already solved cells.

removeSolved :: Transformer
removeSolved puzzle = foldl broadcastSolved puzzle puzzle

broadcastSolved :: Puzzle -> Cell -> Puzzle
broadcastSolved puzzle cell
    | broadcasted cell = puzzle -- already done
    | isUnsolved cell = puzzle -- nothing to broadcast
    | otherwise = applyWhen (samePosWith cell) setBroadcasted bs
    where final = head $ candidates cell
          isApplicable c = isUnsolved c && hasCand final c && intersects c cell
          bs = applyWhen isApplicable (removeCellCandidate final) puzzle

-- Singles, only possibility solver:

solveSingles :: Transformer
solveSingles puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchSingles puzzle) houseSelectors

searchSingles :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchSingles puzzle p = map removerFor singles
    where unsolved = filterWith [p, isUnsolved] puzzle
          singles = filter isSingle $ uniqueCandidates unsolved
          isSingle cand = hasOne $ withCandidate cand unsolved
          removerFor cand = applyWhen (\c -> p c && hasCand cand c) (setCellCandidate cand)

-- Block omission, candidates within one block on one row or column, only.
-- Can remove the possible candidates on that row or column on adjacent block.

applyRemover :: Puzzle -> Transformer -> Puzzle
applyRemover puzzle remover = remover puzzle

solveBlockOmissions :: Transformer
solveBlockOmissions puzzle = foldl applyRemover puzzle (rowRemovers ++ colRemovers)
    where rowRemovers = concatMap (searchBlockOmissionBy rowOf puzzle) [0..8]
          colRemovers = concatMap (searchBlockOmissionBy colOf puzzle) [0..8]

searchBlockOmissionBy :: (Cell -> Index) -> Puzzle -> Index -> [Transformer]
searchBlockOmissionBy indexer puzzle blockIndex = removers
    where unsolved = filterWith [(==blockIndex) . blockOf, isUnsolved] puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint indexer (withCandidate cand unsolved)
          removerFor ind cand = applyWhen (\c -> indexer c == ind && blockOf c /= blockIndex) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

-- Block omission: row or column has a candidate only within one block, any other
-- candidate in that block can be removed.

solveOmitCandidateInOneBlock :: Transformer
solveOmitCandidateInOneBlock puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchOmitCandidateInOneBlock puzzle) (concatMap selectors [rowOf, colOf])
          selectors f = map (\i -> (==i) . f) [0..8]

searchOmitCandidateInOneBlock :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchOmitCandidateInOneBlock puzzle p = removers
    where unsolved = filterWith [p, isUnsolved] puzzle
          candidates = uniqueCandidates unsolved
          check cand = joint blockOf (withCandidate cand unsolved)
          removerFor blockInd cand = applyWhen (\c -> not (p c) && blockOf c == blockInd) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- candidates, isJust $ check c]

-- Solving Naked pair
-- If row, column, or block contains two pairs with exactly same candidates,
-- the numbers in elsewhere within the house can be removed.

solveNakedPairs :: Transformer
solveNakedPairs puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchNakedPair puzzle) houseSelectors

searchNakedPair :: [Cell] -> (Cell -> Bool) -> [Transformer]
searchNakedPair puzzle p = map removerFor $ filterWith [not . null, hasRemovals, isNakedPair] unique_pairs
    where unsolved = filterWith [p, isUnsolved] puzzle
          pair_cells = filter hasPair unsolved
          unique_pairs = unique $ map candidates pair_cells
          isNakedPair pair = hasTwo $ filter (==pair) $ map candidates pair_cells
          hasRemovals pair = any (\cell -> hasAnyCand pair cell && candidates cell /= pair) unsolved
          removerFor pair = applyWhen (\c -> p c && candidates c /= pair) (removeCellCandidates pair)

-- *Main> searchHiddenPair px ((==7) . rowOf)
-- [(2,[65,67]),(8,[65,67])]

solveHiddenPair :: Transformer
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

solveNakedTriplets :: Transformer
solveNakedTriplets puzzle = foldl applyRemover puzzle removers
    where removers = concatMap (searchNakedTriplets puzzle) houseSelectors

searchNakedTriplets :: Puzzle -> (Cell -> Bool) -> [Transformer]
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

searchHiddenTriplet :: Puzzle -> (Cell -> Bool) -> [Transformer]
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

-- X-Wing.

solveXwing :: Transformer
solveXwing puzzle = foldl applyRemover puzzle removers
    where removers = concat [searchXwing puzzle sel cand | cand <- [1..9], sel <- ["rows", "cols"]]

searchXwing :: Puzzle -> String -> Candidate -> [Transformer]
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


-- Swordfish (X-Wing on three columns or rows)

data RowCol = Row | Col deriving (Eq)

--searchSwordfish :: Puzzle -> RowCol -> Candidate -> [Transformer]
searchSwordfish puzzle rowCol cand = selections
    where unsolved = filterWith [hasCand cand, isUnsolved] puzzle
          (op1, op2) = if rowCol == Row then (rowOf, colOf) else (colOf, rowOf)
          selector i = filter ((==i) . op1)
          selectors = map selector [0..8]
          selections = filter (($[2, 3]) . elem . length) $ map ($unsolved) selectors
          validComb comb = (==3) $ length $ unique $ map op2 $ concat comb
          validCombs = filter validComb $ tripletCombinations selections

-- Unique rectangle.

solveUniqueRectangle puzzle = foldl applyRemover puzzle removers
    where removers = searchUniqueRect puzzle

searchUniqueRect puzzle = map removerFor $ filter validComb combs
    where pairs = filter hasPair puzzle
          combs = tripletCombinations pairs
          ops = [rowOf, colOf]
          maxTwoBlock = (<3) . length . unique . map blockOf
          get f = singleFromTriplet . map f
          rectanglePos comb = all (isJust . ($comb) . get) ops
          validComb comb = rectanglePos comb &&allSame (map candidates comb) && maxTwoBlock comb
          removerFor comb = applyWhen ((== pos comb) . cellPos) (removeCellCandidates (candidates $ head comb))
              where pos comb = map (fromJust . ($comb) . get) ops
                    cellPos cell = map ($cell) ops

singleFromTriplet :: Eq p => [p] -> Maybe p
singleFromTriplet [a, b, c]
    | a == b = Just c
    | a == c = Just b
    | b == c = Just a
    | otherwise = Nothing -- "All the elements are unique"
singleFromTriplet _ = error "Works only for lists of length 3."

-- XY wing
-- On any three unique cells (x, y1, y2)
-- x, y1, y2 have only pair candidates
-- x intersects with both y1 and y2
-- y1 does not intersect y2
-- one of candidates in x is in y1 and the other in y2
-- the candidates in y1 and y2 not shared in x are the same

solveXyWing :: Transformer
solveXyWing puzzle = foldl applyRemover puzzle removers
    where removers = map removerFor $ searchXyWing puzzle
          removerFor comb = applyWhen (xyRemovalPos comb) (removeCellCandidate $ xyRemovalCand comb)

xyRemovalPos :: [Cell] -> Cell -> Bool
xyRemovalPos comb cell = all (intersectsEx cell) (drop 1 comb)

xyRemovalCand :: [Cell] -> Candidate
xyRemovalCand [x, y1, _] = head $ filter (`notElem` candidates x) (candidates y1)

searchXyWing :: Puzzle -> [[Cell]]
searchXyWing puzzle = xyCombinations pairs
    where pairs = filter hasPair puzzle

xyPosOk :: Cell -> Cell -> Cell -> Bool
xyPosOk x y1 y2 = intersectsEx x y1 && intersectsEx x y2 && not (intersectsEx y1 y2)

xyCandsOk :: Cell -> Cell -> Cell -> Bool
xyCandsOk x y1 y2 = all threeUnique [[x, y1], [x, y2], [y1, y2], [x, y1, y2]]
    where threeUnique xs = (==3) $ length $ unique $ concatMap candidates xs

xyCombinations :: [Cell] -> [[Cell]]
xyCombinations xs = [[x, y1, y2] 
    | x <- xs
    , (y1, i) <- xsi
    , (y2, j) <- xsi
    , i < j
    , xyPosOk x y1 y2
    , xyCandsOk x y1 y2
    ]
    where xsi = zip xs [0..]