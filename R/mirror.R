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


#' Mirror Fractals
#'
#' Flip fractals horizontally or vertically.
#'
#' @param fractal A fractal object to be mirrored.
#' @param direction The direction of the mirroring, either "horizontal" or
#'   "vertical". Can be abbreviated.
#'
#' @return The mirrored fractal.
#'
#' @examples
#' d <- dragon_curve(10)
#' d_h <- mirror(d, "horizontal")
#' d_v <- mirror(d, "vertical")
#' plot(d)
#' plot(d_h)
#' plot(d_v)
#'
#' @export
mirror <- function(fractal, direction) {
    UseMethod("mirror")
}


#' @rdname mirror
#' @export
mirror.default <- function(fractal,
                           direction = c("horizontal", "vertical")) {
    # For class color_matrix or any other matrix that is not of class
    # dragon_curve.
    direction <- match.arg(direction)
    cls <- class(fractal)
    if (direction == "horizontal") {
        fractal <- fractal[, ncol(fractal):1]
    } else if (direction == "vertical") {
        fractal <- fractal[nrow(fractal):1, ]
    }
    class(fractal) <- cls
    fractal
}


#' @rdname mirror
#' @export
mirror.dragon_curve <- function(fractal,
                                direction = c("horizontal", "vertical")) {
    direction <- match.arg(direction)
    axis <- switch(direction, horizontal = "x", vertical = "y")
    fractal[, axis] <- fractal[, axis] * -1
    fractal
}
