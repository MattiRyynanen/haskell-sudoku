# haskell-sudoku
Simple Sudoku solving with Haskell

Build with

    stack ghc sudoku-solver.hs

and run with

    ./sudoku-solver.hs

or compile and run

    stack runghc sudoku-solver.hs

Building for profiling

    stack ghc -- -prof -fprof-auto -rtsopts sudoku-solver.hs

And then running with profiling

    cat puzzles.txt | ./sudoku-solver +RTS -p