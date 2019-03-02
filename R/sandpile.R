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
# - Add an option to use 8 neighbors per cell. Remember that the limit height is
#   then 8. Do this with an if statement, just put it below the rest.


#' @export
sandpile <- function(height) {
    sidelength <- 11  # must be an odd number
    n <- sidelength ^ 2
    pile <- matrix(0, nrow = sidelength, ncol = sidelength)
    center <- ceiling(sidelength / 2)
    pile[center, center] <- height
    left_border <- seq(1, sidelength)
    increase_by <- 10  # must be an even number

    repeat {
        to_topple <- which(pile > 3)
        if (length(to_topple) == 0) break

        if (any(to_topple %in% left_border)) {
            # Increase the size of the pile
            new_sidelength <- sidelength + 2 * increase_by
            new_pile <- matrix(0, nrow = new_sidelength, ncol = new_sidelength)
            middle <- seq(increase_by + 1, length.out = sidelength)
            coords <- cbind(
                row = rep(middle, sidelength),
                col = rep(middle, each = sidelength)
            )
            new_pile[coords] <- pile
            pile <- new_pile
            sidelength <- new_sidelength
            n <- sidelength ^ 2
            left_border <- seq(1, sidelength)
            to_topple <- which(pile > 3)
        }

        pile[to_topple] <- pile[to_topple] - 4

        top_neighbors <- to_topple - 1
        bottom_neighbors <- to_topple + 1
        left_neighbors <- to_topple - sidelength
        right_neighbors <- to_topple + sidelength

        pile[top_neighbors] <- pile[top_neighbors] + 1
        pile[bottom_neighbors] <- pile[bottom_neighbors] + 1
        pile[left_neighbors] <- pile[left_neighbors] + 1
        pile[right_neighbors] <- pile[right_neighbors] + 1
    }
    pile
}


#' @export
color_sandpile <- function(pile, color_fun, num_neighbors) {
    if (missing(num_neighbors)) {
        # Try to guess the number of neighbors used for creating the sandpile:
        if (max(pile) > 3) {
            num_neighbors <- 8
        } else {
            num_neighbors <- 4
        }
    } else if (!(num_neighbors %in% c(4, 8))) {
        stop("num_neighbors must be either 4 or 8.")
    }

    sand_colors <- data.frame(
        height = seq(0, num_neighbors - 1),
        color = color_fun(num_neighbors),
        stringsAsFactors = FALSE
    )
    matrix(sand_colors$color[match(pile, sand_colors$height)],
           nrow = nrow(pile), ncol = ncol(pile))
}
