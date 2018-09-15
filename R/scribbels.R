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

  # Check which points are inside the cardioid or the period-2 bulb:
  x <- Re(complex_plane)
  y <- Im(complex_plane)
  q <- (x - 0.25)^2 + y^2
  inside <- q * (q + (x - 0.25)) < 0.25 * y^2
  n_steps[inside] <- as.integer(max_iterations)

  # Note to future me: I tried to implement periodicity checking but
  # it didn't improve performance.

  todo <- !inside

  for (i in 1:max_iterations) {
    todo[todo] <- abs(z[todo]) < threshold
    n_steps[todo] <- n_steps[todo] + 1L
    z[todo] <- z[todo] ^ 2 + complex_plane[todo]
    if (!any(todo)) break
  }

  # TODO: I should return here if the user does not want smooth colors.
  # Just return a matrix with the discrete colors.

  # Smooth colouring, after some pseudocode taken from
  # https://en.wikipedia.org/wiki/Mandelbrot_set#Continuous_(smooth)_coloring
  # Don't ask me how that works.
  outside <- n_steps < max_iterations
  n_steps[outside] <- n_steps[outside] + 1 - log(log(abs(z[outside]), 2), 2)

  # TODO: use the [outside] subset more below to maybe speed things up? So that
  # not the whole million element matrix interpolates colors.

  color_fun <- colorRampPalette(c(rgb(0, 0, 0.5), "white", rgb(1, 0.75, 0),
                                  "darkred", "black"))
  colors <- color_fun(max_iterations)

  # Prepare for interpolation:
  colors_m <- sub("#", "", colors)
  colors_m <- strsplit(colors_m, "")
  colors_m <- lapply(  # paste together characters at even and odd positions
    colors_m,
    function(x) paste0(x[c(TRUE, FALSE)], x[c(FALSE, TRUE)])
  )
  colors_m <- lapply(colors_m, strtoi, base = 16)
  colors_m <- do.call(rbind, colors_m)

  color_1 <- colors_m[floor(n_steps), ]
  color_2 <- colors_m[ceiling(n_steps), ]
  d <- n_steps %% 1  # fractional par

  # Color interpolation:
  color_1_r <- matrix(colors_m[floor(n_steps), 1], nrow = nrow(d))
  color_1_g <- matrix(colors_m[floor(n_steps), 2], nrow = nrow(d))
  color_1_b <- matrix(colors_m[floor(n_steps), 3], nrow = nrow(d))
  color_2_r <- matrix(colors_m[ceiling(n_steps), 1], nrow = nrow(d))
  color_2_g <- matrix(colors_m[ceiling(n_steps), 2], nrow = nrow(d))
  color_2_b <- matrix(colors_m[ceiling(n_steps), 3], nrow = nrow(d))

  col_min <- pmin(color_1_r, color_2_r)
  col_max <- pmax(color_1_r, color_2_r)
  col_ip_r <- round(col_min + (col_max - col_min) * d)


  col_min <- pmin(color_1_g, color_2_g)
  col_max <- pmax(color_1_g, color_2_g)
  col_ip_g <- round(col_min + (col_max - col_min) * d)

  col_min <- pmin(color_1_b, color_2_b)
  col_max <- pmax(color_1_b, color_2_b)
  col_ip_b <- round(col_min + (col_max - col_min) * d)
  col_ip <- paste0(
    "#",
    paste0(as.hexmode(col_ip_r), as.hexmode(col_ip_g), as.hexmode(col_ip_b))
  )
  col_ip <- matrix(col_ip, nrow = nrow(d))
  col_ip
}


# Which values of threshold and max_iterations produce the best results?
# Which combinations are the same?


m2 <- mandelbrot(1200, 800, threshold = 2, max_iterations = 500)

png("test_smooth_5.png", width = ncol(m2), height = nrow(m2))
grid.raster(m2, interpolate = FALSE)
dev.off()



