# Copyright (C) 2020 Sebastian Henz
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see https://www.gnu.org/licenses/.


test_that("L-system plant generates correctly", {
    l_plant <- l_system(
        axiom = "X",
        rules = list(
            X = "F+[[X]-X]-F[-FX]+X",
            F = "FF"
        ),
        n = 7,
        angle = pi * 0.15,
        initial_angle = pi * 0.45
    )
    expect_identical(l_plant, test_l_plant)
})


test_that("L-system dragon curve generates correctly", {
    l_dragon <- l_system(
        axiom = "FX",
        rules = list(
            X = "X+YF+",
            Y = "-FX-Y"
        ),
        n = 12,
        angle = pi / 2
    )
    expect_identical(l_dragon, test_l_dragon)
})


test_that("L-system sierpinski triangle generates correctly", {
    l_triangle <- l_system(
        axiom = "F-G-G",
        rules = list(
            F = "F-G+F+G-F",
            G = "GG"
        ),
        n = 6,
        angle = 2 * pi / 3,
        initial_angle = pi / 3
    )
    expect_identical(l_triangle, test_l_triangle)
})


test_that("L-system line length change and angle flip work", {
    l_line_length_angle_flip <- l_system(
        axiom = "X",
        rules = list(X = "F[+@.7X]F![-@.6X]F"),
        n = 9,
        angle = pi * 0.125
    )
    expect_identical(l_line_length_angle_flip, test_l_line_length_angle_flip)
})
