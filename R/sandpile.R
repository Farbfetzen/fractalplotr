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


#' Sandpile
#'
#' Drop some sand in the center and see it spread outwards.
#'
#' @param n The number of grains dropped in the center.
#' @param colors A vector of length 4 or `NULL`. Specifies the colors to be
#'   allocated to the values `0:4`. If `NULL`, the number of grains
#'   per cell are returned instead. Defaults to a grey palette.
#'
#' @return A matrix of class "color_matrix", either containing colors or the
#'   number of grains per cell, depending on the value of `colors`.
#'
#' @seealso [plot.color_matrix()]
#'
#' @examples
#' s <- sandpile(1000)
#' plot(s)
#'
#' @export
sandpile <- function(n, colors = grey.colors(4, start = 1, end = 0)) {
    stopifnot(n > 0)
    n <- as.integer(n)
    sidelength <- 11L
    increase_by <- 11L
    pile <- matrix(0L, sidelength, sidelength)
    pile[sidelength, sidelength] <- n
    size <- length(pile)
    almost_right_border <- size - sidelength
    right_border <- seq(almost_right_border + 1L, size)
    bottom_border <- seq(sidelength, size, sidelength)
    far_bottom <- bottom_border + 1L
    toppling <- which(pile > 3L)
    while (length(toppling) > 0L) {
        if (any(toppling <= sidelength)) {
            # The sand in the first column of the matrix is about to topple.
            # Increase the size of the pile so that the grains don't spill out.
            pile <- rbind(matrix(0L, increase_by, sidelength), pile)
            sidelength <- sidelength + increase_by
            pile <- cbind(matrix(0L, sidelength, increase_by), pile)

            size <- length(pile)
            right_border <- seq(size - sidelength + 1L, size)
            almost_right_border <- size - sidelength
            bottom_border <- seq(sidelength, size, sidelength)
            far_bottom <- bottom_border + 1L
            toppling <- which(pile > 3L)
        }

        k <- pile[toppling] %/% 4L
        pile[toppling] <- pile[toppling] %% 4L


        top_neighbors <- toppling - 1L
        left_neighbors <- toppling - sidelength
        right_neighbors <- toppling + sidelength
        right_filter <- right_neighbors <= size
        right_neighbors <- right_neighbors[right_filter]
        k_right <- k[right_filter]
        bottom_neighbors <- toppling + 1L
        bottom_filter <- !bottom_neighbors %in% far_bottom
        bottom_neighbors <- bottom_neighbors[bottom_filter]
        k_bottom <- k[bottom_filter]

        pile[top_neighbors] <- pile[top_neighbors] + k
        pile[left_neighbors] <- pile[left_neighbors] + k
        pile[right_neighbors] <- pile[right_neighbors] + k_right
        pile[bottom_neighbors] <- pile[bottom_neighbors] + k_bottom

        # To use the symmetry I have to handle the borders in a special way.
        # Because the right border and the bottom border, which are the middle
        # column and middle row in the final matrix, can be filled from the
        # right or the bottom. So I look if sand flows into the right border
        # from the left and then I add sand again because it would also flow
        # into it from the right. Same with the bottom border but for this I
        # just copy the right border.
        right_again_filter <- right_neighbors > almost_right_border
        if (any(right_again_filter)) {
            right_border_again <- right_neighbors[right_again_filter]
            k_right_again <- k_right[right_again_filter]
            pile[right_border_again] <- pile[right_border_again] + k_right_again
            pile[bottom_border] <- pile[right_border]
            if (size %in% right_neighbors) {
                # Also add one to the center
                pile[size] <- pile[size] + k_right_again[length(k_right_again)]
            }
        }

        toppling <- which(pile > 3L)
    }

    # trim the edges solely consisting of zeroes:
    pile <- pile[rowSums(pile) > 0, colSums(pile) > 0, drop = FALSE]

    # Make the quarter matrix whole:
    if (n > 3L) {
        pile <- cbind(pile, rotate(pile, -1)[, -1])
        pile <- rbind(pile, mirror(pile, "vertical")[-1, ])
    }

    if (!is.null(colors)) {
        pile <- matrix(colors[pile + 1L], nrow = nrow(pile))
        class(pile) <- c("color_matrix", class(pile))
    }

    pile
}
