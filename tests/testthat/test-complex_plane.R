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


test_that("complex plane is correctly created", {
    m1 <- matrix(
        c(-1 + 4i, 0 + 4i, 1 + 4i, 2 + 4i,
          -1 + 3i, 0 + 3i, 1 + 3i, 2 + 3i,
          -1 + 2i, 0 + 2i, 1 + 2i, 2 + 2i),
        nrow = 3,
        byrow = TRUE
    )
    m2 <- make_complex_plane(4, 3, 3, 2, 0.5 + 3i)
    expect_identical(m1, m2)
})
