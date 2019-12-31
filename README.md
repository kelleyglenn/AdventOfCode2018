# AdventOfCode2018
Solutions to https://adventofcode.com/2018

Each package contains the solution to the puzzles for a given day on the advent calendar. After completing the first puzzle, if the follow-up puzzle required significant changes to the solution to the first puzzle, I created a new class for the second puzzle.

All testing is done with scalatest, including the final solution to the puzzle. The final solution was usually found by writing a final assert that expects some meaningless value. When scalatest reports the failure, it displays the actual answer. After I submitted to the actual answer to the puzzle web page, I updated the expected result to match, so any future changes will be regression tested.
