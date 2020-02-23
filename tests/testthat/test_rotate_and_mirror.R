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


test_that("color_matrix is rotated as expected", {
    m1 <- matrix(1:9, nrow = 3, byrow = TRUE)
    m2 <- matrix(c(7, 4, 1, 8, 5, 2, 9, 6, 3), nrow = 3, byrow = TRUE)
    m3 <- matrix(c(9, 8, 7, 6, 5, 4, 3, 2, 1), nrow = 3, byrow = TRUE)
    m4 <- matrix(c(3, 6, 9, 2, 5, 8, 1, 4, 7), nrow = 3, byrow = TRUE)
    class(m1) <- c("color_matrix", class(m1))
    class(m2) <- c("color_matrix", class(m2))
    class(m3) <- c("color_matrix", class(m3))
    class(m4) <- c("color_matrix", class(m4))
    expect_equal(rotate(m1), m2)
    expect_equal(rotate(m1, 2), m3)
    expect_equal(rotate(m1, 3), m4)
    expect_equal(rotate(m1, 4), m1)
    expect_equal(rotate(m1, -1), m4)
    expect_equal(rotate(m1, -2), m3)
    expect_equal(rotate(m1, -3), m2)
    expect_equal(rotate(m1, -4), m1)
})


test_that("dragon_curve is rotated as expected", {
    m1 <- matrix(1:8, ncol = 2)
    m2 <- matrix(c(5:8, -(1:4)), ncol = 2)
    m3 <- matrix(c(-(1:8)), ncol = 2)
    m4 <- matrix(c(-(5:8), 1:4), ncol = 2)
    class(m1) <- c("dragon_curve", "matrix")
    class(m2) <- c("dragon_curve", "matrix")
    class(m3) <- c("dragon_curve", "matrix")
    class(m4) <- c("dragon_curve", "matrix")
    colnames(m1) <- c("x", "y")
    colnames(m2) <- c("x", "y")
    colnames(m3) <- c("x", "y")
    colnames(m4) <- c("x", "y")
    expect_equal(rotate(m1), m2)
    expect_equal(rotate(m1, 2), m3)
    expect_equal(rotate(m1, 3), m4)
    expect_equal(rotate(m1, 4), m1)
    expect_equal(rotate(m1, -1), m4)
    expect_equal(rotate(m1, -2), m3)
    expect_equal(rotate(m1, -3), m2)
    expect_equal(rotate(m1, -4), m1)
})


test_that("color_matrix is mirrored as expected", {
    m1 <- matrix(1:9, nrow = 3, byrow = TRUE)
    mh <- matrix(c(3, 2, 1, 6, 5, 4, 9, 8, 7), nrow = 3, byrow = TRUE)
    mv <- matrix(c(7, 8, 9, 4, 5, 6, 1, 2, 3), nrow = 3, byrow = TRUE)
    class(m1) <- c("color_matrix", "matrix")
    class(mh) <- c("color_matrix", "matrix")
    class(mv) <- c("color_matrix", "matrix")
    expect_equal(mirror(m1, "horizontal"), mh)
    expect_equal(mirror(m1, "horiz"), mh)
    expect_equal(mirror(m1, "h"), mh)
    expect_equal(mirror(m1, "vertical"), mv)
    expect_equal(mirror(m1, "vert"), mv)
    expect_equal(mirror(m1, "v"), mv)
})


test_that("dragon_curve is mirrored as expected", {
    m1 <- matrix(1:8, ncol = 2)
    mh <- matrix(c(-1:-4, 5:8), ncol = 2)
    mv <- matrix(c(1:4, -5:-8), ncol = 2)
    class(m1) <- c("dragon_curve", "matrix")
    class(mh) <- c("dragon_curve", "matrix")
    class(mv) <- c("dragon_curve", "matrix")
    colnames(m1) <- c("x", "y")
    colnames(mh) <- c("x", "y")
    colnames(mv) <- c("x", "y")
    expect_equal(mirror(m1, "horizontal"), mh)
    expect_equal(mirror(m1, "horiz"), mh)
    expect_equal(mirror(m1, "h"), mh)
    expect_equal(mirror(m1, "vertical"), mv)
    expect_equal(mirror(m1, "vert"), mv)
    expect_equal(mirror(m1, "v"), mv)
})
