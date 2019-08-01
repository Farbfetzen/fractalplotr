#' Dragon Curve
#'
#' TODO: this documentation
#'
#' The length of the curve will be 2 ^ order - 1.
#'
#' @param order foo bar
#'
#' @return foo bar
#'
#' @references \url{https://en.wikipedia.org/wiki/Dragon_curve}
#'
#' @examples
#' dragon_curve(5)
#'
#' @export
dragon_curve <- function(order) {
    stopifnot(
        is.numeric(order),
        order > 1
    )
    curve <- 1
    for (i in 2:order) {
        len <- length(curve)
        middle_index <- (len + 1) / 2
        curve <- c(curve, 1, curve)
        curve[len + 1 + middle_index] <- -1
    }


    dragon <- matrix(0, nrow = length(curve) + 2, ncol = 2)
    colnames(dragon) <- c("x", "y")
    dragon[2, ] <- c(1, 0)
    directions <- list(
        c(1, 0),  # right
        c(0, 1),  # up
        c(-1, 0), # left
        c(0, -1)  # down
    )
    direction <- 0
    i <- 3
    for (turn in curve) {
        direction <- (direction + turn) %% 4
        # +1 because in R indices start at 1
        dragon[i, ] <- dragon[i - 1, ] + directions[[direction + 1]]
        i <- i + 1
    }
    class(dragon) <- c("dragon_curve", class(dragon))
    invisible(dragon)
}
