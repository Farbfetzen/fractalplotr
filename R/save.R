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


#' Save a fractal as an image
#'
#' TODO: Fill in description of the methods for color_matrix and dragon_curve.
#'
#' @param fractal The fractal to be saved.
#' @param filename A character string naming a file path.
#' @param ... Further arguments to \code{png()} or \code{plot_fractals()}
#'
#' @export
save_fractal <- function(fractal, ...) {
    UseMethod("save_fractal", fractal)
}


#' @rdname save_fractal
save_fractal.color_matrix <- function(color_matrix, filename) {
    png(filename, width = ncol(color_matrix), height = nrow(color_matrix))
    on.exit(invisible(dev.off()))
    plot_fractal(color_matrix)
}


#' @rdname save_fractal
save_fractal.dragon_curve <- function(dragon, color, filename, ...) {
    png(filename, ...)
    on.exit(invisible(dev.off()))
    par(mar = rep(0, 4))
    plot_fractal(dragon, color)
}
