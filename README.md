# haskell-sudoku
Simple Sudoku solving with Haskell

Build with

    stack build

and run (and build possible changes) with

    stack run

Building for profiling

    stack ghc -- -prof -fprof-auto -rtsopts sudoku-solver.hs

And then running with profiling

    cat puzzles.txt | ./sudoku-solver +RTS -p