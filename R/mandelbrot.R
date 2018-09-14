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
