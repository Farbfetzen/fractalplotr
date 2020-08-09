## code to prepare `test_data` dataset goes here


# L-system plant
l_system <- grow_l_system(
    axiom = "X",
    rules = list(
        `X` = "F+[[X]-X]-F[-FX]+X",
        `F` = "FF"
    ),
    n = 7
)
test_l_plant <- convert_l_system(
    instructions = l_system,
    angle = pi * 0.15,
    initial_angle = pi * 0.45
)


# L-system dragon curve
l_system <- grow_l_system(
    axiom = "FX",
    rules = list(
        `X` = "X+YF+",
        `Y` = "-FX-Y"
    ),
    n = 12
)
test_l_dragon <- convert_l_system(
    instructions = l_system,
    angle = pi / 2
)


# L-system sierpinski triangle
l_system <- grow_l_system(
    axiom = "F-G-G",
    rules = list(
        `F` = "F-G+F+G-F",
        `G` = "GG"
    ),
    n = 6)
test_l_triangle <- convert_l_system(
    instructions = l_system,
    angle = 2 * pi / 3,
    initial_angle = pi / 3
)


# mandelbrot set
test_mandelbrot <- mandelbrot(
    width = 150,
    height = 100,
    re_width = 3,
    max_iterations = 128,
    threshold = 2,
    return_colors = FALSE
)


# sandpile
test_sandpile <- sandpile(10000, NULL)


usethis::use_data(
    test_l_plant,
    test_l_dragon,
    test_l_triangle,
    test_mandelbrot,
    test_sandpile,
    internal = TRUE,
    overwrite = TRUE
)
