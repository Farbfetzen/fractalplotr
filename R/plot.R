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
