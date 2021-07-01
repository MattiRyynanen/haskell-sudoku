import Definitions
import Printers
import Solvers
import SamplePuzzles

pl = loadPuzzle SamplePuzzles.level5_hs_2020_06_28
solutions = solve [idleStep pl "The loaded puzzle."]
px = result $ head solutions
