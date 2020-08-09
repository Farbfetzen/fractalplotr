
## candidates
* [Julia set](https://en.wikipedia.org/wiki/Julia_set)
* [Ulam-Warburton cellular automaton](https://en.wikipedia.org/wiki/Ulam%E2%80%93Warburton_automaton)
* [toothpick pattern](https://en.wikipedia.org/wiki/Toothpick_sequence)


## notes
- Make a NEWS.md file
- remove the wikipedia links. only include non-trivial references

# some interesting L-systems
axiom = "FX"
rules = list(
    `X` = "FX[+FX][-FX]",
    `F` = "FF"
)
angle <- pi / 4
n <- 7
axiom = "FX"
rules = list(
    `X` = "FX[FFX][+FX][-FX]",
    `F` = "FF"
)
angle <- pi / 3
n <- 6
axiom = "X"
rules = list(
    `F` = "FF",
    `X` = "F+X"
)
angle <- pi / 5
n <- 7
axiom = "X"
rules = list(
    `F` = "FF",
    `X` = "F[X][+X][-X]"
)
angle <- pi / 2
n <- 9
