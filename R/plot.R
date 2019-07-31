#' Plot fractals
#'
#' @name plot_fractal
#'
#' @param x A fractal object.
#' @param ... Futher arguments passed on to \code{plot()}. Only used for dragon
#'   curves.
#'
#' @examples
#' plot(dragon_curve(12), col = "orange")
NULL


#' @rdname plot_fractal
#' @param pretty The color of the dragon curve.
#' @export
plot.dragon_curve <- function(x, pretty = TRUE, ...) {
    if (pretty) {
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        par(mar = rep(0, 4))
        plot.default(x, type = "l", asp = 1, axes = FALSE, ann = FALSE, ...)
    } else {
        plot.default(x, ...)
    }
}


#' @rdname plot_fractal
#' @export
plot.color_matrix <- function(x, ...) {
    grid::grid.newpage()
    grid::grid.raster(x, interpolate = FALSE)
}
