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


mandelbrot <- function(width, height, re_min= -2, re_max = 1,
                       im_min = -1i, im_max = 1i,
                       center = -0.5+0i, re_width, im_height,
                       max_iterations = 128, threshold = 2,
                       prevent_distortion = TRUE) {
  # TODO:
  # - check input: if prevent_distortion only some of the arguments should be
  #   given
  complex_plane <- make_complex_plane(width, height,
                                      re_min, re_max,
                                      im_min, im_max)
  n_steps <- z <- matrix(
    0L,
    nrow = nrow(complex_plane),
    ncol = ncol(complex_plane)
  )
  repeat {
    todo <- n_steps < max_iterations & abs(z) < threshold
    z[todo] <- z[todo] ^ 2 + complex_plane[todo]
    n_steps[todo] <- n_steps[todo] + 1L
    if (!any(todo)) break
  }
  n_steps
}


plot_mandelbrot <- function(m, filename) {
  # Picture dimension = matrix dimension in pixels
  # Plots it as a png.

  # TODO: Accept a color palette for this function and remove the color_fun
  # and color_palette lines. Maybe remove also the color_matrix and
  # create it elsewhere.


  color_fun <- colorRampPalette(c("black", "red", "yellow", "green", "blue", "white"))
  color_palette <- color_fun(max(m))
  color_matrix <- matrix(color_palette[m], nrow = nrow(m))

  on.exit(dev.off())
  png(filename, width = ncol(m), height = nrow(m))
  grid.raster(color_matrix, interpolate = FALSE)
}
