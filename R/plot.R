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


#' Plot fractals
#'
#' @name plot_fractal
#'
#' @param x A fractal object.
#' @param ... Futher arguments passed on to \code{plot()}. Only used for dragon
#'   curves.
#'
#' @examples
#' plot(dragon_curve(12), col = "orange")
NULL


#' @rdname plot_fractal
#' @param pretty TODO: Explain what this does exactly. Ant to set it to false if
#'   the user wants to customize the plot.
#' @export
plot.dragon_curve <- function(x, pretty = TRUE, ...) {
    if (pretty) {
        plot.default(x, type = "l", asp = 1, axes = FALSE, ann = FALSE, ...)
    } else {
        plot.default(x, ...)
    }
}


#' @rdname plot_fractal
#' @export
plot.color_matrix <- function(x, ...) {
    grid.newpage()
    grid.raster(x, interpolate = FALSE)
}
