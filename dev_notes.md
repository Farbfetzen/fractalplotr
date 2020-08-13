
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


Example for "modify line length" and "flip direction":
https://www.reddit.com/r/creativecoding/comments/i93yk4/growing_tree_lsystem/g1ciem1/
Remember to include them in save and load instructions.

Example for line length modification:
https://www.reddit.com/r/generative/comments/i4mjs6/fractal_tree_transformation/g0j78tg/
but I replaced the IQ2 with 0.7071
axiom <- "X"
rules <- list(`X` = "[@0.7071-FX][@0.7071+FX]")
angle <- pi * 0.2  # Change between 0 and pi
n <- 10
plot_l_system(convert_l_system(grow_l_system(axiom, rules, n), angle))

