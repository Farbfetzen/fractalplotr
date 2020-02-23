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


test_that("sandpile has not changed for small piles", {
    # "small" meaning that the intitial size of the pile does not increase
    s1 <- matrix(
        c(0, 0, 0, 1, 2, 1, 0, 0, 0,
          0, 0, 3, 2, 0, 2, 3, 0, 0,
          0, 3, 0, 3, 2, 3, 0, 3, 0,
          1, 2, 3, 0, 3, 0, 3, 2, 1,
          2, 0, 2, 3, 0, 3, 2, 0, 2,
          1, 2, 3, 0, 3, 0, 3, 2, 1,
          0, 3, 0, 3, 2, 3, 0, 3, 0,
          0, 0, 3, 2, 0, 2, 3, 0, 0,
          0, 0, 0, 1, 2, 1, 0, 0, 0),
        nrow = 9
    )
    expect_identical(sandpile(100, NULL), s1)
    s2 <- matrix("lightgray")
    class(s2) <- c("color_matrix", class(s2))
    expect_equal(sandpile(1), s2)
    expect_equal(sum(sandpile(100, NULL)), 100)
})


# TODO: Add test for large piles where the size does increase. Maybe n = 1000.
# Use the inst directory like that mandelbrot test.
