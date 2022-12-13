library(adventofcode22)
x <- readLines("./inst/input09.txt")

p1 <- f09a(x[1:1000])
p2 <- f09b(x)

stopifnot(p1 == aoc_solutions$day09a)
stopifnot(p2 == aoc_solutions$day09b)
