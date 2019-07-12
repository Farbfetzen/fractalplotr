#' Save a fractal as an image
#'
#' TODO: Fill in description of the methods for color_matrix and dragon_curve.
#'
#' @param fractal The fractal to be saved.
#' @param filename A character string naming a file path.
#' @param ... Further arguments to \code{png()} or \code{plot_fractals()}
#'
#' @export
save_fractal <- function(fractal, ...) {
    UseMethod("save_fractal", fractal)
}


#' @rdname save_fractal
save_fractal.color_matrix <- function(color_matrix, filename) {
    png(filename, width = ncol(color_matrix), height = nrow(color_matrix))
    on.exit(invisible(dev.off()))
    plot_fractal(color_matrix)
}


#' @rdname save_fractal
save_fractal.dragon_curve <- function(dragon, color, filename, ...) {
    png(filename, ...)
    on.exit(invisible(dev.off()))
    par(mar = rep(0, 4))
    plot_fractal(dragon, color)
}
