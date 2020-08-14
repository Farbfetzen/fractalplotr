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


#' Dragon Curve
#'
#' Repeatedly fold a line to create a space-filling curve.
#'
#' @param n How often the curve will be folded. Must be at least 1.
#'
#' @return A matrix of class "dragon_curve" with two columns for the x and y
#'   coordinates of the folds.
#'
#' @examples
#' d <- dragon_curve(9)
#' plot(d)
#'
#' @seealso [plot.dragon_curve()]
#'
#' @export
dragon_curve <- function(n) {
    stopifnot(
        is.numeric(n),
        n > 0
    )
    curve <- c()
    for (i in seq_len(n)) {
        len <- length(curve)
        middle_index <- (len + 1) / 2
        curve <- c(curve, 1, curve)
        if (i > 1) {
            curve[len + 1 + middle_index] <- -1
        }
    }

    # Convert the folds to coordinates:
    dragon <- matrix(0, nrow = length(curve) + 2, ncol = 2)
    colnames(dragon) <- c("x", "y")
    dragon[2, ] <- c(1, 0)
    directions <- list(
        c(1, 0),  # right
        c(0, 1),  # up
        c(-1, 0), # left
        c(0, -1)  # down
    )
    direction <- 0
    i <- 3
    for (turn in curve) {
        direction <- (direction + turn) %% 4
        # +1 because in R indices start at 1
        dragon[i, ] <- dragon[i - 1, ] + directions[[direction + 1]]
        i <- i + 1
    }
    class(dragon) <- c("dragon_curve", class(dragon))
    dragon
}


#' Plot Dragon Curves
#'
#' Plot dragon curves as lines.
#'
#' @param x A matrix of class "dragon_curve" as returned from [dragon_curve()]
#'   with columns x and y.
#' @param ... Other parameters passed to [`lines()`][graphics::lines],
#'   e.g. `col` or `lwd`.
#'
#' @return None
#'
#' @examples plot(dragon_curve(9))
#'
#' @export
plot.dragon_curve <- function(x, ...) {
    plot(
        NA,
        xlim = range(x[, "x"]),
        ylim = range(x[, "y"]),
        asp = 1,
        xaxs = "i",
        yaxs = "i",
        axes = FALSE,
        ann = FALSE
    )
    lines(x, ...)
}
