# foo <- mandelbrot(1200, 800)
#
# opar <- par(no.readonly = TRUE)
#
#
# foo_norm <- (foo - min(foo)) / (max(foo) - min(foo))
# rasterImage(foo_norm, xleft = 0, ybottom = 0)
#
# grid.raster(foo_norm, interpolate = FALSE, x = 0, y = 0)
#
#
# foo_r <- rotate_matrix(foo, "right")
#
# color_fun <- colorRampPalette(c("black", "red", "yellow", "green", "blue", "white"))
# color_palette <- color_fun(max(foo))
#
# system.time({
# png("test1.png", width = ncol(foo), height = nrow(foo))
# par(mar = c(0, 0, 0, 0))
# image(foo_r, col = color_palette, useRaster = TRUE, axes = FALSE, ann = FALSE)
# dev.off()
# })
#
# library(grid)
# bla <- matrix(color_palette[foo], nrow = nrow(foo))
#
# system.time({
# png("test2.png", width = ncol(foo), height = nrow(foo))
# par(mar = c(0, 0, 0, 0))
# grid.raster(bla, interpolate = FALSE)
# dev.off()
# })
#
#
# a <- matrix(1:4, nrow = 2, byrow = TRUE)
# a
#
# matrix(letters[a], nrow = nrow(a))
#
#
# # Conclusion: use grid.raster from the grid package because it is faster and
# # more convenient. Convert the numbers to colors beforehand. Try to implement
# # smooth scaling to avoyd the colored bands. Maybe the mandelbrot() function
# # should also return either z or a matrix of color strings?
# # Always use interpolate = TRUE.
