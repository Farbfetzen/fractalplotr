
## candidates
* [Julia set](https://en.wikipedia.org/wiki/Julia_set)
* [Ulam-Warburton cellular automaton](https://en.wikipedia.org/wiki/Ulam%E2%80%93Warburton_automaton)
* [toothpick pattern](https://en.wikipedia.org/wiki/Toothpick_sequence)


## notes
- Make a NEWS.md file
- remove the wikipedia links. only include non-trivial references
- add function to convert from degrees to radians

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


Example for line length modification:
axiom <- "X"
rules <- list(`X` = "[@.7071-FX][@.7071+FX]")
angle <- pi * 0.2  # Change between 0 and pi
n <- 10
plot_l_system(convert_l_system(grow_l_system(axiom, rules, n), angle))


Example for "modify line length" and "flip direction":
axiom <- "X"
rules <- list(`X` = "F[+@.7X]F![-@.6X]F")
angle <- pi * 0.125
n <- 10
plot_l_system(convert_l_system(grow_l_system(axiom, rules, n), angle))


https://www.reddit.com/r/generative/comments/i58xim/120%C2%BA_fractal_tree_lsystem/g0nfm17/
axiom <- "[FX]++[FX]"
rules <- list(`X` = "[@.7071+[FX]++[FX]]")
angle <- pi * 2 / 3
n <- 13
foo <- convert_l_system(grow_l_system(axiom, rules, n), angle, 0)
col <- terrain.colors(1000)
plot_l_system(foo, col = col)


l_system <- grow_l_system(
    axiom = "X",
    rules = list(
        `X` = "F+[[X]-X]-F[-FX]+X",
        `F` = "FF"
    ),
    n = 7
)
l_lines <- convert_l_system(
    instructions = l_system,
    angle = pi * 0.15,
    initial_angle = pi * 0.45
)
plot_l_system(
    l_lines = l_lines,
    col = terrain.colors(100)
)
