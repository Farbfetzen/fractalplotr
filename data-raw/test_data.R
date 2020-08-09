## code to prepare `test_data` dataset goes here

l_system <- grow_l_system(
    axiom = "X",
    rules = list(
        `X` = "F+[[X]-X]-F[-FX]+X",
        `F` = "FF"
    ),
    n = 7
)
l_plant <- convert_l_system(
    instructions = l_system,
    angle = pi * 0.15,
    initial_angle = pi * 0.45
)

# dragon curve:
l_system <- grow_l_system(
    axiom = "FX",
    rules = list(
        `X` = "X+YF+",
        `Y` = "-FX-Y"
    ),
    n = 12
)
l_dragon <- convert_l_system(
    instructions = l_system,
    angle = pi / 2
)

# sierpinski triangle:
l_system <- grow_l_system(
    axiom = "F-G-G",
    rules = list(
        `F` = "F-G+F+G-F",
        `G` = "GG"
    ),
    n = 6)
l_triangle <- convert_l_system(
    instructions = l_system,
    angle = 2 * pi / 3,
    initial_angle = pi / 3
)


usethis::use_data(
    l_plant,
    l_dragon,
    l_triangle,
    internal = TRUE,
    overwrite = TRUE
)
