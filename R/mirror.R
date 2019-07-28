#' Mirror Fractals
#'
#' Mirror fractals horizontally or vertically.
#'
#' @param fractal A fractal object to be mirrored
#' @param direction The direction of the mirroring, either "horizontal" or
#'   "vertical". Can be abbreviated.
#'
#' @return The mirrored fractal.
#' @export
#'
#' @examples
#' d1 <- dragon_curve(10)
#' d2 <- mirror(d, "horizontal")
#' plot(d1)
#' plot(d2)
mirror <- function(fractal, direction) {
    UseMethod("mirror")
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
    print(axis)
    fractal[, axis] <- fractal[, axis] * -1
    fractal
}


#' @rdname mirror
#' @export
mirror.color_matrix <- function(fractal,
                                direction = c("horizontal", "vertical")) {
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
