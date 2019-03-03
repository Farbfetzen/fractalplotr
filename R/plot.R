# Copyright (C) 2019 Sebastian Henz
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
# along with this program. If not, see <https://www.gnu.org/licenses/>.


#' Plot fractals
#'
#' @param fractal The fractal to be plotted.
#' @param color The color of the dragon curve as single value in a format
#'     that R understands.
#'
#' @export
#'
#' @examples
#' plot_fractal(dragon_curve(12), "orange")
plot_fractal <- function(fractal, ...) {
    UseMethod("plot_fractal", fractal)
}


#' @rdname plot_fractal
#' @export
plot_fractal.color_matrix <- function(color_matrix) {
    grid::grid.newpage()
    grid::grid.raster(color_matrix, interpolate = FALSE)
}


#' @rdname plot_fractal
#' @export
plot_fractal.dragon_curve <- function(dragon, color = "black") {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mar = rep(0, 4))
    plot(dragon, col = color, type = "l", asp = 1,
         xaxt = "n", yaxt = "n", bty = "n", ann = FALSE
    )
}
