# aoc-2022-haskell

Advent of Code 2022 in Haskell

![Build Status](https://github.com/benoitpas/aoc-2022/workflows/build/badge.svg)

As usual for AoC, the error handling is minimal as the programs are only designed to handle the provided inputs.

# Day 12

For fun I generated the images for the terrain (with the path):

![terrain](terrain.bmp)

And the distance from the start:

![heatmap](heatmap.bmp)

# Day 14

To learn about parser combinator, I have decided to use the 'parsec' library to parse the input file instead of using my usual way based on 'split'. The following resources were quite helpful:
* https://book.realworldhaskell.org/read/using-parsec.html
* https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27222
