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
    # "small" meaning that the initial matrix does not increase in size
    reference <- matrix(
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
    mode(reference) <- "integer"
    expect_identical(sandpile(100, NULL), reference)
})


test_that("sandpile hast not changed for large piles", {
    # "large" meaning that the final matrix is bigger than the initial one
    s <- sandpile(10000, NULL)
    expect_identical(s, test_sandpile)
})

# TODO: test with very small n, like 1, 2, 3, 4, 5, 10, 15.
# TODO: test colors
