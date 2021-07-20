module Solvers where

import Data.Maybe
import Data.List (find)

import Definitions
import SolverDefinitions
import Snippets
import PerHouseSolvers

solvers :: [Solver]
solvers = let s = Solver in [
    s removeSolved RemoveBySolved 0,
    s solveSingles Singles 0,
    s solveBlockOmissions BlockOmission 1,
    s solveOmitCandidateInOneBlock OmitCandidateInOneBlock 1,
    s solveNakedPairs NakedPair 2,
    s solveHiddenPairs HiddenPair 2,
    s solveNakedTriplets NakedTriplet 3,
    s solveHiddenTriplet HiddenTriplet 4,
    s solveXwing XWing 5,
    s solveUniqueRectangle UniqueRectangle 5,
    s solveXyWing XYWing 6,
    s pairContradictionSolver PairContradiction 8]

beginSolve :: Puzzle -> [SolutionStep]
beginSolve p = solve [IdleStep p Initial]

lastResult :: [SolutionStep] -> IdleStepId
lastResult = fromJust . getStepId . head

finalResult :: Puzzle -> IdleStepId
finalResult = lastResult . beginSolve

solve :: [SolutionStep] -> [SolutionStep]
solve steps
    | null steps = error "Nothing to solve."
    | not $ housesOk latest changedHousesFromPrevious = addIdleStep InvalidSolution
    | hasZeroCandidates latest = addIdleStep InvalidSolution
    | all isSolved latest = addIdleStep Solved
    | isJust simplestSolver = solve $ prepend $ stepFor b
    | otherwise = addIdleStep NoSolution
    where latest = getPuzzle $ head steps -- the latest puzzle
          simplestSolver = find (not . null . snd) $ map transFor solvers
          prepend s = s : steps
          b = fromJust simplestSolver
          precedingPuzzle = getPuzzle $ if length steps > 1 then steps !! 1 else head steps
          changedHousesFromPrevious = housesOf (changedCells latest precedingPuzzle)
          transFor solv = (solv, transformers solv latest)
          stepFor solv_trs = SolverStep (applyRemovers latest (snd solv_trs)) latest (fst solv_trs)
          addIdleStep stepId = prepend $ IdleStep latest stepId

changedCells :: Puzzle -> Puzzle -> [Cell]
changedCells cur pre = [c | (c, p) <- zip cur pre, c /= p]

housesOf :: [Cell] -> [Cell -> Bool]
housesOf cells = concatMap houseSel [rowOf, colOf, blockOf]
    where houseSel f = [(==i) . f | i <- unique $ map f cells]

-- Remove candidates based on already solved cells.

removeSolved :: Puzzle -> [Transformer]
removeSolved = concatMap broadcastSolved

broadcastSolved :: Cell -> [Transformer]
broadcastSolved cell
    | broadcasted cell = [] -- already done
    | isUnsolved cell = [] -- nothing to broadcast
    | otherwise = [remover, broadcastFlagSetter]
    where final = head $ candidates cell
          isApplicable c = isUnsolved c && hasCand final c && intersects cell c
          remover = applyWhen isApplicable (removeCellCandidate final)
          broadcastFlagSetter = updateAt (index cell) setBroadcasted

-- Block omission, candidates within one block on one row or column, only.
-- Can remove the possible candidates on that row or column on adjacent block.

solveBlockOmissions :: Puzzle -> [Transformer]
solveBlockOmissions puz = rowRemovers ++ colRemovers
    where rowRemovers = concatMap (searchBlockOmissionBy rowOf puz) [0..8]
          colRemovers = concatMap (searchBlockOmissionBy colOf puz) [0..8]

searchBlockOmissionBy :: (Cell -> Index) -> Puzzle -> Index -> [Transformer]
searchBlockOmissionBy indexer puz blockIndex = removers
    where unsolved = filterWith [(==blockIndex) . blockOf, isUnsolved] puz
          cands = uniqueCandidates unsolved
          check cand = joint indexer (withCandidate cand unsolved)
          removerFor ind cand = applyWhen (\c -> indexer c == ind && blockOf c /= blockIndex) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- cands, isJust $ check c]

-- Block omission: row or column has a candidate only within one block, any other
-- candidate in that block can be removed.

solveOmitCandidateInOneBlock :: Puzzle -> [Transformer]
solveOmitCandidateInOneBlock puz = removers
    where removers = concatMap (searchOmitCandidateInOneBlock puz) (concatMap selectors [rowOf, colOf])
          selectors f = map (\i -> (==i) . f) [0..8]

searchOmitCandidateInOneBlock :: Puzzle -> (Cell -> Bool) -> [Transformer]
searchOmitCandidateInOneBlock puz p = removers
    where unsolved = filterWith [p, isUnsolved] puz
          cands = uniqueCandidates unsolved
          check cand = joint blockOf (withCandidate cand unsolved)
          removerFor blockInd cand = applyWhen (\c -> not (p c) && blockOf c == blockInd) (removeCellCandidate cand)
          removers = [removerFor (fromJust $ check c) c | c <- cands, isJust $ check c]

data RowCol = Row | Col deriving (Eq)

-- X-Wing.

solveXwing :: Puzzle -> [Transformer]
solveXwing puz = concat [searchXwing puz sel cand | cand <- [1..9], sel <- [Row, Col]]

searchXwing :: Puzzle -> RowCol -> Candidate -> [Transformer]
searchXwing puz rowCol cand
    | length unsolved <= 4 = []
    | otherwise = map removerFor (concatMap removalCells validCombs)
    where unsolved = filterWith [hasCand cand, isUnsolved] puz
          ops = if rowCol == Row then (rowOf, colOf) else (colOf, rowOf)
          selector i = map (snd ops) $ filter ((==i) . fst ops) unsolved
          twoPos f = [(i, f i) | i <- [0..8], hasTwo $ f i]
          validComb comb = allSame $ map snd comb
          validCombs = map (\comb -> (map fst comb, head $ map snd comb)) $ filter validComb (pairCombinations $ twoPos selector)
          removalCells comb = filter (\c -> snd ops c `elem` snd comb && fst ops c `notElem` fst comb && hasCand cand c) unsolved
          removerFor cell = applyWhen (samePosWith cell) (removeCellCandidate cand)

-- Swordfish (X-Wing on three columns or rows)

--searchSwordfish :: Puzzle -> RowCol -> Candidate -> [Transformer]
searchSwordfish :: [Cell] -> RowCol -> Int -> [[[Cell]]]
searchSwordfish puz rowCol cand = validCombs
    where unsolved = filterWith [hasCand cand, isUnsolved] puz
          (op1, op2) = if rowCol == Row then (rowOf, colOf) else (colOf, rowOf)
          selector i = filter ((==i) . op1)
          selectors = map selector [0..8]
          selections = filter (($[2, 3]) . elem . length) $ map ($unsolved) selectors
          validComb comb = (==3) $ length $ unique $ map op2 $ concat comb
          validCombs = filter validComb $ tripletCombinations selections

-- Unique rectangle.

solveUniqueRectangle :: Puzzle -> [Transformer]
solveUniqueRectangle puz = map removerFor $ filter validComb combs
    where pairs = filter hasPair puz
          combs = tripletCombinations pairs
          ops = [rowOf, colOf]
          maxTwoBlock = (<3) . length . unique . map blockOf
          get f = singleFromTriplet . map f
          rectanglePos comb = all (isJust . ($comb) . get) ops
          validComb comb = rectanglePos comb && allSame (map candidates comb) && maxTwoBlock comb && hasRemoval comb
          posFor comb = map (fromJust . ($comb) . get) ops
          cellPos cell = map ($cell) ops
          hasRemoval comb = hasAnyCand (candidates $ head comb) (head $ filter ((== posFor comb) . cellPos) puz)
          removerFor comb = applyWhen ((== posFor comb) . cellPos) (removeCellCandidates (candidates $ head comb))


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

solveXyWing :: Puzzle -> [Transformer]
solveXyWing puz = map removerFor $ searchXyWing puz
    where removerFor comb = applyWhen (xyRemovalPos comb) (removeCellCandidate $ xyRemovalCand comb)

xyRemovalPos :: [Cell] -> Cell -> Bool
xyRemovalPos comb cell = all (intersectsEx cell) (drop 1 comb)

xyRemovalCand :: [Cell] -> Candidate
xyRemovalCand [x, y1, _] = head $ filter (`notElem` candidates x) (candidates y1)
xyRemovalCand _ = error "Available for three cells only."

searchXyWing :: Puzzle -> [[Cell]]
searchXyWing puz = xyCombinations pairs
    where pairs = filter hasPair puz

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
    where xsi = zip xs [0 :: Int ..]

-- Pair contradiction solver.
-- Pick a pair. Try to solve with a candidate, if
-- there's a InvalidSolution, it must be the other one.

pairContradictionSolver :: Puzzle -> [Transformer]
pairContradictionSolver puz
    | null pairCells = []
    | otherwise = if finalResult (p1 puz) == Solved then [p1] else [p2]
    where pairCells = take 1 $ filter hasPair puz
          pairCell = head pairCells
          cands = candidates pairCell
          solveWith cand = applyWhen (samePosWith pairCell) (setCellCandidate cand)
          p1 = solveWith (head cands) :: Transformer
          p2 = solveWith (last cands) :: Transformer
