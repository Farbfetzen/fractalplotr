#' Dragon Curve
#'
#' TODO: this documentation
#'
#' The length of the curve will be 2 ^ order - 1.
#'
#' @param order foo bar
#'
#' @return foo bar
#' @export
#'
#' @examples
#' dragon_curve(5)
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





#' Flip a Dragon Curve
#'
#' Mirror a dragon curve either horizontally or vertically.
#'
#' @param dragon The dragon curve returned by \code{dragon_curve()}.
#' @param direction A string specifying the direction of the flip. Possible
#'   values are "horizontal" and "vertical".
#'
#' @return The mirrored dragon curve coordinates.
#' @export
#'
#' @family dragon curve functions
#'
#' @examples
#' d <- dragon_curve(3)
#' flip_dragon(d, "horizontal")
flip_dragon <- function(dragon, direction) {
    if (direction == "horizontal") {
        dragon[, "x"] <- dragon[, "x"] * -1
    } else if (direction == "vertical") {
        dragon[, "y"] <- dragon[, "y"] * -1
    } else {
        stop("Direction must be either 'horizontal' or 'vertical'.")
    }
    dragon
}


#' Rotate a Dragon Curve
#'
#' Rotate dragon curve coordinates 90° to the right (clockwise) or left
#' (counterclockwise) once or multiple times.
#'
#' @param dragon A matrix with two columns for the x and y coordinates of
#'   the folds in the curve. Usually created by
#'   \code{\link{dragon_curve}}.
#' @param direction A string specifying the direction of rotation. Possible
#'   values are "left", "right", "clockwise" and "counterclockwise".
#' @param times A single positive number specifying how often to rotate by 90°.
#'
#' @return A matrix containing the rotated coordinates.
#' @export
#'
#' @family dragon curve functions
#'
#' @examples
#' d <- dragon_curve(3)
#' rotate_dragon(d, "left")
rotate_dragon <- function(dragon, direction, times = 1) {
    cls <- class(dragon)
    for (i in seq_len(times)) {
        if (direction %in% c("right", "clockwise")) {
            x <- dragon[, "y"]
            y <- -dragon[, "x"]
        } else if (direction %in% c("left", "counterclockwise")) {
            x <- -dragon[, "y"]
            y <- dragon[, "x"]
        }
        dragon <- cbind(x, y)
    }
    class(dragon) <- dragon
    dragon
}
