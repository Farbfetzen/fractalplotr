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


context("mandelbrot set")


test_that("mandelbrot set is calculated correctly", {
    filepath <- system.file("testdata", "mandelbrot.csv",
                            package = "fractalplotr", mustWork = TRUE)
    m_reference <- as.matrix(read.table(filepath, sep = ","))
    dimnames(m_reference) <- NULL
    m <- mandelbrot(width = 150, height = 100, re_width = 3,
                    max_iterations = 128, threshold = 2, colors = NULL)
    expect_identical(m, m_reference)
})

# TODO: Test coordinate arguments and their combinations. re_width and im_height
# should never be provided together, for example.
