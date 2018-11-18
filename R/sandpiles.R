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

# Numberphile video on sandpiles: https://www.youtube.com/watch?v=1MtEUErz7Gg
# This is the version with the infinite grid mentioned at the end of the video.
# Lots of sand is put in the center and then the rules are applied until there
# are no more stacks to topple.

# TODO:
# - Use the symmetry to increase computation speed.
# - Make a version with 9 neighbors per cell. Remember that the limit is then 8.
#   Do this with an if statement, just put it below the rest.


# library(grid)
#
#
# sidelength <- 31  # must be an odd number
# height <- 300000
#
# n <- sidelength ^ 2
# sandpile <- matrix(0, nrow = sidelength, ncol = sidelength)
# center <- ceiling(sidelength / 2)
# sandpile[center, center] <- height
# left_border <- seq(1, sidelength)
# increase_by <- 10  # must be an even number
#
# repeat {
#     to_topple <- which(sandpile > 3)
#     if (length(to_topple) == 0) break
#
#     if (any(to_topple %in% left_border)) {
#         # Increase the size of the pile
#         new_sidelength <- sidelength + 2 * increase_by
#         new_pile <- matrix(0, nrow = new_sidelength, ncol = new_sidelength)
#         middle <- seq(increase_by + 1, length.out = sidelength)
#         coords <- cbind(
#             row = rep(middle, sidelength),
#             col = rep(middle, each = sidelength)
#         )
#         new_pile[coords] <- sandpile
#         sandpile <- new_pile
#         sidelength <- new_sidelength
#         n <- sidelength ^ 2
#         left_border <- seq(1, sidelength)
#         to_topple <- which(sandpile > 3)
#     }
#
#     sandpile[to_topple] <- sandpile[to_topple] - 4
#
#     top_neighbors <- to_topple - 1
#     bottom_neighbors <- to_topple + 1
#     left_neighbors <- to_topple - sidelength
#     right_neighbors <- to_topple + sidelength
#
#     sandpile[top_neighbors] <- sandpile[top_neighbors] + 1
#     sandpile[bottom_neighbors] <- sandpile[bottom_neighbors] + 1
#     sandpile[left_neighbors] <- sandpile[left_neighbors] + 1
#     sandpile[right_neighbors] <- sandpile[right_neighbors] + 1
# }
#
# sand_colors <- data.frame(
#     num = 0:3,
#     color = rev(grey.colors(4)),
#     stringsAsFactors = FALSE
# )
# color_matrix <- matrix(sand_colors$color[match(sandpile, sand_colors$num)],
#                        nrow = nrow(sandpile), ncol = ncol(sandpile))
# png(paste0("sandpile_", format(height, scientific = FALSE), ".png"),
#     width = ncol(sandpile), height = nrow(sandpile))
# grid.raster(color_matrix, interpolate = FALSE)
# dev.off()
