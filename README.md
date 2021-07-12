# haskell-sudoku
Simple Sudoku solving with Haskell

Build with

    stack build

and run (and build possible changes) with

    stack run

Building for profiling

    stack build --profile

And then running with profiling

    cat puzzles.txt | stack exec --profile haskell-sudoku.exe session -- +RTS -p
