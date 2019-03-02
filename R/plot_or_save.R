# fractalplotr - Plot Beautiful Fractals with R
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


#' @export
plot_fractal <- function(fractal) {
    grid::grid.newpage()
    grid::grid.raster(fractal, interpolate = FALSE)
}


#' @export
save_fractal <- function(fractal, file) {
    png(file, width = ncol(fractal), height = nrow(fractal))
    plot_fractal(fractal)
    invisible(dev.off())
}
