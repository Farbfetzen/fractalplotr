# w <- 1200
# h <- 800
# mi <- 1000
#
#
# mandelbrot_new <- function(width, height, re_min= -2, re_max = 1,
#                        im_min = -1i, im_max = 1i,
#                        center = -0.5+0i, re_width, im_height,
#                        max_iterations = 128, threshold = 2,
#                        prevent_distortion = TRUE) {
#   complex_plane <- make_complex_plane(width, height,
#                                       re_min, re_max,
#                                       im_min, im_max)
#   n_steps <- z <- matrix(
#     0L,
#     nrow = nrow(complex_plane),
#     ncol = ncol(complex_plane)
#   )
#
#   # Check which points are inside the cardioid or the period-2 bulb:
#   x <- Re(complex_plane)
#   y <- Im(complex_plane)
#   q <- (x - 0.25)^2 + y^2
#   inside <- q * (q + (x - 0.25)) < 0.25 * y^2
#   n_steps[inside] <- as.integer(max_iterations)
#
#   # Note for future me: I tried to implement periodicity checking but
#   # it didn't improve performance.
#
#   todo <- !inside
#
#   for (i in 1:max_iterations) {
#     todo[todo] <- abs(z[todo]) < threshold
#     n_steps[todo] <- n_steps[todo] + 1L
#     z[todo] <- z[todo] ^ 2 + complex_plane[todo]
#     if (!any(todo)) break
#   }
#   n_steps
# }
#
#
# system.time({
#   orig <- mandelbrot(w, h, max_iterations = mi)
# })
#
# system.time({
#   new <- mandelbrot_new(w, h, max_iterations = mi)
# })
#
# identical(orig, new)
