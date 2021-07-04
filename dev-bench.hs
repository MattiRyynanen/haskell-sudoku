import Definitions
import Printers
import Solvers
import SamplePuzzles
import Data.Maybe

pl = loadPuzzle SamplePuzzles.level5_hs_2020_07_05
solutions = solve [IdleStep (fromJust pl) Initial]
px = getPuzzle $ head solutions
