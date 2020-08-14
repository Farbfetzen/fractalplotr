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


#' Rotate Fractals
#'
#' Rotate a fractal by multiples of 90 degrees.
#'
#' @param fractal The fractal to be rotated.
#' @param n An integer specifying the number and direction of turns. A positive
#'   value turns counterclockwise and a negative value turns clockwise.
#'
#' @return The rotated fractal.
#'
#' @examples
#' d <- dragon_curve(10)
#' d_l <- rotate(d, 1)  # rotate once to the left
#' d_r <- rotate(d, -2)  # rotate twice to the right
#' plot(d)
#' plot(d_l)
#' plot(d_r)
#'
#' @export
rotate <- function(fractal, n) {
    UseMethod("rotate")
}


#' @rdname rotate
#' @export
rotate.default <- function(fractal, n) {
    # For class color_matrix or any other matrix that is not of class
    # dragon_curve.
    clockwise <- n < 0
    cls <- class(fractal)
    for (i in seq_len(abs(n))) {
        if (clockwise) {
            fractal <- t(fractal[nrow(fractal):1, ])
        } else {
            fractal <- t(fractal[, ncol(fractal):1])
        }
    }
    class(fractal) <- cls
    fractal
}


#' @rdname rotate
#' @export
rotate.dragon_curve <- function(fractal, n) {
    clockwise <- n < 0
    for (i in seq_len(abs(n))) {
        # I could just reverse the order of the colnames instead of the columns
        # itself. But maybe users rely on their order, that x is column 1 and y
        # is column 2.
        fractal[, c("x", "y")] <- fractal[, c("y", "x")]
        if (clockwise) {
            fractal[, "y"] <- -fractal[, "y"]
        } else {
            fractal[, "x"] <- -fractal[, "x"]
        }
    }
    fractal
}
