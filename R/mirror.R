#' Mirror Fractals
#'
#' Mirror fractals horizontally or vertically.
#'
#' @param fractal A fractal object to be mirrored
#' @param direction The direction of the mirroring, either "horizontal" or
#'   "vertical". Can be abbreviated.
#'
#' @return The mirrored fractal.
#'
#' @examples
#' d <- dragon_curve(10)
#' d_h <- mirror(d, "horizontal")
#' d_v <- mirror(d, "vertical")
#' plot(d)
#' plot(d_h)
#' plot(d_v)
#'
#' @export
mirror <- function(fractal, direction) {
    UseMethod("mirror")
}


#' @rdname mirror
#' @export
mirror.default <- function(fractal,
                           direction = c("horizontal", "vertical")) {
    # For class color_matrix or any other matrix that is not of class
    # dragon_curve.
    direction <- match.arg(direction)
    cls <- class(fractal)
    if (direction == "horizontal") {
        fractal <- fractal[, ncol(fractal):1]
    } else if (direction == "vertical") {
        fractal <- fractal[nrow(fractal):1, ]
    }
    class(fractal) <- cls
    fractal
}


#' @rdname mirror
#' @export
mirror.dragon_curve <- function(fractal,
                                direction = c("horizontal", "vertical")) {
    direction <- match.arg(direction)
    axis <- switch (direction,
        horizontal = "x",
        vertical = "y"
    )
    fractal[, axis] <- fractal[, axis] * -1
    fractal
}
