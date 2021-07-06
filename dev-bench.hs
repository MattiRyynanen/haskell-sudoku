import Definitions
import Printers
import Solvers
import SamplePuzzles
import Data.Maybe

pl = loadPuzzle "..68.5..7 2.......3 ..5..6.2. .....9.6. ..4...5.. .7.5..... .9.4..1.. 1.......4 4..9.78.."--SamplePuzzles.level5_hs_2020_07_05
solutions = beginSolve (fromJust pl)
px = getPuzzle $ head solutions
