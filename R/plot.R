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
#' @param pretty TODO: Explain what this does exactly. Ant to set it to false if
#'   the user wants to customize the plot.
#' @export
plot.dragon_curve <- function(x, pretty = TRUE, ...) {
    if (pretty) {
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
