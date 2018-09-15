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


# For the continuois color interpolation (see wikipedia):


blubb <- colorRampPalette(c("red", "green"))(5)
# "#FF0000" "#BF3F00" "#7F7F00" "#3FBF00" "#00FF00"

foo <- sub("#", "", blubb)
foo <- strsplit(foo, "")
foo <- lapply(foo, function(x) paste0(x[c(TRUE, FALSE)], x[c(FALSE, TRUE)]))
foo <- lapply(foo, strtoi, base = 16)
foo <- do.call(rbind, foo)

# say I want to interpolate between the first two colors in foo at 0.7 between them:
c1 <- foo[1, ]
c2 <- foo[2, ]
d <- 0.7
c_i <- apply(rbind(c1, c2), 2, function(x) min(x) + diff(range(x)) * d)
c_i <- pmin(255, pmax(0, round(c_i)))
c_i <- paste0("#", paste(as.hexmode(c_i), collapse = ""))

