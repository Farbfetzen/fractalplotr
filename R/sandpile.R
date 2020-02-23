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


# TODO: Add an option to use 8 neighbors per cell. Remember that this changes
# the limit n to 8. Adjust the documentation accordingly. The color vector must
# be of length 8. Can I use the same symmetry speed improvements as with n = 4?
# TODO: Improve the speed of this function. See issue #14.

#' Sandpile
#'
#' Drop some sand in the center and see it spread outwards.
#'
#' @param n The number of grains dropped in the center.
#' @param colors A vector of length 4 or \code{NULL}. Specifies the colors to be
#'   allocated to the values \code{0:4}. If \code{NULL}, the number of grains
#'   per cell are returned instead.
#'
#' @return A matrix, either containing colors or the number of grains per cell,
#'   depending on the argument \code{colors}.
#'
#' @references \url{https://en.wikipedia.org/wiki/Abelian_sandpile_model}
#'
#' @examples
#' s <- sandpile(1000)
#' plot(s)
#'
#' @export
sandpile <- function(n, colors = c("white", "lightgray", "darkgray", "black")) {
    stopifnot(
        n > 0
    )
    sidelength <- 11
    size <- sidelength * sidelength
    right_border <- seq(size - sidelength + 1, size)
    almost_right_border <- min(right_border) - 1
    far_right <- right_border + sidelength
    min_far_right <- min(far_right)
    bottom_border <- seq(sidelength, size, sidelength)
    far_bottom <- bottom_border + 1
    pile <- matrix(0, sidelength, sidelength)
    pile[sidelength, sidelength] <- n
    increase_by <- 10  # must be an even number
    to_topple <- which(pile > 3)
    while (length(to_topple) > 0) {
        if (any(to_topple <= sidelength)) {
            # The sand in the first column of the matrix is about to topple.
            # Increase the size of the pile so that the grains don't spill out.
            pile <- rbind(
                matrix(0, increase_by, sidelength),
                pile
            )
            sidelength <- sidelength + increase_by
            pile <- cbind(
                matrix(0, sidelength, increase_by),
                pile
            )
            size <- sidelength * sidelength
            right_border <- seq(size - sidelength + 1, size)
            almost_right_border <- min(right_border) - 1
            far_right <- right_border + sidelength
            min_far_right <- min(far_right)
            bottom_border <- seq(sidelength, size, sidelength)
            far_bottom <- bottom_border + 1
            to_topple <- which(pile > 3)
        }

        pile[to_topple] <- pile[to_topple] - 4

        # To use the symmetry I have to handle the borders in a special way.
        # Because the right border and the bottom border, which are the middle
        # column and middle row in the final matrix, can be filled from the
        # right or the bottom. So I look if sand flows into the right border
        # from the left and then I add sand again because it would also flow
        # into it from the right. Same with the bottom border but for this I
        # just copy the right border.

        # FIXME: Come back later and think of a better way to do this.
        top_neighbors <- to_topple - 1
        left_neighbors <- to_topple - sidelength
        right_neighbors <- to_topple + sidelength
        right_neighbors <- right_neighbors[right_neighbors < min_far_right]
        bottom_neighbors <- to_topple + 1
        bottom_neighbors <- bottom_neighbors[!bottom_neighbors %in% far_bottom]

        pile[top_neighbors] <- pile[top_neighbors] + 1
        pile[left_neighbors] <- pile[left_neighbors] + 1
        pile[right_neighbors] <- pile[right_neighbors] + 1
        pile[bottom_neighbors] <- pile[bottom_neighbors] + 1

        right_border_again <- right_neighbors[
            right_neighbors > almost_right_border
        ]
        if (length(right_border_again) > 0) {
            pile[right_border_again] <- pile[right_border_again] + 1
            pile[bottom_border] <- pile[right_border]
            if (size %in% right_neighbors) {
                pile[size] <- pile[size] + 1
            }
        }

        to_topple <- which(pile > 3)
    }

    # trim the edges solely consisting of zeroes:
    pile <- pile[rowSums(pile) > 0, colSums(pile) > 0, drop = FALSE]

    # Make the quarter matrix whole:
    if (n > 3) {
        pile <- cbind(pile, rotate(pile)[, -1])
        pile <- rbind(pile, mirror(pile, "vertical")[-1, ])
    }

    if (is.null(colors)) {
        return(pile)
    }

    pile <- matrix(colors[pile + 1], nrow = nrow(pile))

    class(pile) <- c("color_matrix", class(pile))
    pile
}
