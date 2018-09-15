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
