# Copyright (C) 2018 Sebastian Henz
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
# along with this program. If not, see http://www.gnu.org/licenses.


context("matrix creation and manipulation")


test_that("complex plane is correctly created", {
    m1 <- matrix(
        c(-1+4i, 0+4i, 1+4i, 2+4i,
          -1+3i, 0+3i, 1+3i, 2+3i,
          -1+2i, 0+2i, 1+2i, 2+2i),
        nrow = 3,
        byrow = TRUE
    )
    m2 <- make_complex_plane(4, 3, 3, 2, 0.5+3i)
    expect_identical(m1, m2)
})


m1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
m2 <- matrix(c(7, 4, 1, 8, 5, 2, 9, 6, 3), nrow = 3, byrow = TRUE)
m3 <- matrix(c(9, 8, 7, 6, 5, 4, 3, 2, 1), nrow = 3, byrow = TRUE)
m4 <- matrix(c(3, 6, 9, 2, 5, 8, 1, 4, 7), nrow = 3, byrow = TRUE)

test_that("matrix is rotated clockwise", {
    expect_identical(m2, rotate_matrix(m1, "right", 1))
    expect_identical(m3, rotate_matrix(m1, "clockwise", 2))
    expect_identical(m4, rotate_matrix(m1, "right", 3))
    expect_identical(m1, rotate_matrix(m1, "clockwise", 4))
})

test_that("matrix is rotated counterclockwise", {
    expect_identical(m4, rotate_matrix(m1, "left", 1))
    expect_identical(m3, rotate_matrix(m1, "counterclockwise", 2))
    expect_identical(m2, rotate_matrix(m1, "left", 3))
    expect_identical(m1, rotate_matrix(m1, "counterclockwise", 4))
})

test_that("matrix is flipped as expected", {
    mh <- matrix(c(3, 2, 1, 6, 5, 4, 9, 8, 7), nrow = 3, byrow = TRUE)
    mv <- matrix(c(7, 8, 9, 4, 5, 6, 1, 2, 3), nrow = 3, byrow = TRUE)
    expect_identical(mh, flip_matrix(m1, "horizontal"))
    expect_identical(mv, flip_matrix(m1, "vertical"))
})
