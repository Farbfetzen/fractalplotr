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


test_that("mandelbrot set has not changed", {
    m <- mandelbrot(width = 150, height = 100, re_width = 3,
                    max_iterations = 128, threshold = 2, return_colors = FALSE)
    expect_identical(m, test_mandelbrot)
})

# TODO: Test coordinate arguments and their combinations. re_width and im_height
# should never be provided together, for example.

# TODO: test colouring
