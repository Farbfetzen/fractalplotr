#' @export
dragon_curve <- function(order, limit = 20) {
    dragon <- fold_dragon(order, limit)
    dragon <- get_dragon_coordinates(dragon)
    class(dragon) <- c(class(dragon), "dragon_curve")
    invisible(dragon)
}


fold_dragon <- function(order, limit = 20) {
    # Note: The length of the curve will be 2 ^ order - 1.
    if (order > limit) {
        stop("The order exceeds the limit and the resulting vector would ",
             "probably be very big in memory. This is because the length is ",
             "2 ^ order - 1. Change the value of \"limit\" to get higher ",
             "order dragon curves.", call. = FALSE)
    }
    if (order == 1) return(1)
    curve <- 1
    for (i in 2:order) {
        middle_index <- ceiling(length(curve) / 2)
        curve_2 <- curve
        curve_2[middle_index] <- -1
        curve <- c(curve, 1, curve_2)
    }
    curve
}


get_dragon_coordinates <- function(turns) {
    coordinates <- matrix(rep(0, (length(turns) + 2) * 2), ncol = 2)
    colnames(coordinates) <- c("x", "y")
    coordinates[2, ] <- c(1, 0)
    directions <- list(
        c(1, 0),  # right
        c(0, 1),  # up
        c(-1, 0), # left
        c(0, -1)  # down
    )
    direction <- 0
    i <- 2
    for (turn in turns) {
        direction <- (direction + turn) %% 4
        coordinates[i + 1, ] <- coordinates[i, ] + directions[[direction + 1]]
        i <- i + 1
    }
    coordinates
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
    cls <- class(dragon)
    if (direction == "horizontal") {
        dragon[, "x"] <- dragon[, "x"] * -1
    } else if (direction == "vertical") {
        dragon[, "y"] <- dragon[, "y"] * -1
    }
    class(dragon) <- dragon
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
