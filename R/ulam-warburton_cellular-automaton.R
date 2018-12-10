# fractalplotr - Plot Beautiful Fractals with R
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


# Note: idea from this video: https://www.youtube.com/watch?v=_UtCli1SgjI


plot_cells <- function(m) {
    m <- abs(m - 1)  # change 0 to 1 and 1 to 0
    grid::grid.newpage()
    grid::grid.raster(m, interpolate = FALSE)
}

# n <- 256
# sidelength <- 2 * n - 1
# m <- matrix(0, nrow = sidelength, ncol = sidelength)
# m[ceiling(sidelength/2), ceiling(sidelength/2)] <- 1
#
#
# timing <- numeric(n - 1)
# len_new <- numeric(n - 1)
# for (i in seq_len(n - 1)) {
#     # start <- Sys.time()
#     left <- cbind(m[, -1], 0)
#     right <- cbind(0, m[, -sidelength])
#     top <- rbind(m[-1, ], 0)
#     bottom <- rbind(0, m[-sidelength, ])
#     new <- left + right + top + bottom + m
#     new[new > 1] <- 0
#     m <- m + new
#     m[m > 1] <- 1
#     timing[i] <- Sys.time() - start
#     len_new[i] <- sum(new)
#     plot_cells(m)
#     Sys.sleep(0.1)
# }
#
# plot(timing, type = "o")
# plot(len_new, type = "o")
# plot_cells(new)
# plot_cells(m)


