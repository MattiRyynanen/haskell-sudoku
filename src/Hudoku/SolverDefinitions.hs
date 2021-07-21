module Hudoku.SolverDefinitions where

import Hudoku.Definitions ( Puzzle )

type Transformer = Puzzle -> Puzzle

data SolverId = RemoveBySolved | Singles
    | BlockOmission | OmitCandidateInOneBlock
    | NakedPair | HiddenPair
    | NakedTriplet | HiddenTriplet
    | XWing | XYWing
    | UniqueRectangle
    | PairContradiction deriving (Eq, Show)

data Solver = Solver { transformer :: Transformer, identifier :: SolverId, level :: Int }
instance Show Solver where show s = concat [show $ level s, ": ", show $ identifier s]

data IdleStepId = Initial | Solved | NoSolution | InvalidSolution deriving (Eq, Ord, Show)

data SolutionStep =
    IdleStep { puzzle :: Puzzle, step :: IdleStepId } | 
    SolverStep { result :: Puzzle, previous :: Puzzle, solver :: Solver } deriving (Show)

getPuzzle :: SolutionStep -> Puzzle
getPuzzle IdleStep { puzzle = p }  = p
getPuzzle SolverStep { result = p } = p

getStepId :: SolutionStep -> Maybe IdleStepId
getStepId (IdleStep _ stepId) = Just stepId
getStepId SolverStep {} = Nothing

applyWhileReduced :: Eq t => (t -> t) -> t -> t
applyWhileReduced f puz = if reduced == puz then puz else applyWhileReduced f reduced
    where reduced = f puz

applyRemover :: Puzzle -> Transformer -> Puzzle
applyRemover puz remover = remover puz

applyRemovers :: Puzzle -> [Transformer] -> Puzzle
applyRemovers = foldl applyRemover
