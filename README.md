# Sudoku

A Haskell implementation of [A Pencil-and-Paper Algorithm for Solving Sudoku Puzzles](http://www.ams.org/notices/200904/rtx090400460p.pdf) by J. F. Crook.

Sudoku puzzles from [Peter Norvig](http://norvig.com/sudoku.html).

## Building

Clone the repo and build using [Stack](https://www.haskellstack.org/)

```bash
stack build
```
## Running

Read and solve puzzles from stdin:

```bash
cat hardest.txt | stack exec sudoku
```
