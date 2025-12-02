# Advent of Code 2025 (Haskell)

To run code for a day:

    cabal run y2025d01 input/2025/day01/sample.txt

## Day 1

- Haskell's `scanl` is handy for this sort of problem.
- I had a bunch of off-by-ones on part 2, for example if you landed exactly on zero, I'd double-count it.
- Looking at reddit posts, the cleanest solution is to replace the big turns with a bunch of L1s and R1s. Then you can reuse your code from part 1. `replicate` is the function for this.

## Day 2

- Defining local helper functions with `where` is handy, and you don't need to write explicit types for them.
- You can't write `1 / 2` in Haskell, but you can write <code>1 `div` 2</code>.
- The Haskell equivalent of `str * n` is `concat (replicate n str)`.
