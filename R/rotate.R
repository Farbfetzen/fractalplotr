#' Rotate Fractals
#'
#' Rotate a fractal either clockwise or counterclockwise by multiples of 90
#' degrees.
#'
#' @param fractal The fractal to be rotated.
#' @param n A positive or negative integer specifying the number and direction
#'   of 90 degree turns. A positive value turns clockwise and a negative value
#'   turns counterclockwise.
#'
#' @return The rotated fractal.
#'
#' @examples
#' d <- dragon_curve(10)
#' d_r <- rotate(d, 1)  # rotate once to the right
#' d_l <- rotate(d, -1)  # rotate once to the left
#' plot(d)
#' plot(d_r)
#' plot(d_l)
#'
#' @export
rotate <- function(fractal, n) {
    UseMethod("rotate")
}


#' @rdname rotate
#' @export
rotate.dragon_curve <- function(fractal, n = 1) {
    clockwise <- n > 0
    for (i in seq_len(abs(n))) {
        # I could just reverse the order of the colnames instead of the columns
        # itself. But maybe users rely on their order, that x is column 1 and y
        # is column 2.
        fractal[, c("x", "y")] <- fractal[, c("y", "x")]
        if (clockwise) {
            fractal[, "y"] <- -fractal[, "y"]
        } else {
            fractal[, "x"] <- -fractal[, "x"]
        }
    }
    fractal
}


#' @rdname rotate
#' @export
rotate.color_matrix <- function(fractal, n = 1) {
    clockwise <- n > 0
    cls <- class(fractal)
    for (i in seq_len(abs(n))) {
        if (clockwise) {
            fractal <- t(fractal[nrow(fractal):1, ])
        } else {
            fractal <- t(fractal[, ncol(fractal):1])
        }
    }
    class(fractal) <- cls
    fractal
}
