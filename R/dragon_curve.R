# Copyright (C) 2018 Sebastian Henz
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses.


#' @export
dragon_curve <- function(order, limit = 20) {
    dragon <- fold_dragon(order, limit)
    dragon <- get_dragon_coordinates(dragon)
    class(dragon) <- "dragon_curve"
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
#' @param coordinates A matrix with two columns for the x and y coordinates of
#'   the folds in the curve. Usually created by
#'   \code{\link{dragon_curve}}.
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
flip_dragon <- function(coordinates, direction) {
    if (direction == "horizontal") {
        coordinates[, "x"] <- coordinates[, "x"] * -1
    } else if (direction == "vertical") {
        coordinates[, "y"] <- coordinates[, "y"] * -1
    }
    coordinates
}


#' Rotate a Dragon Curve
#'
#' Rotate dragon curve coordinates 90° to the right (clockwise) or left
#' (counterclockwise) once or multiple times.
#'
#' @param coordinates A matrix with two columns for the x and y coordinates of
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
#' d <- dragon_curve(fold_dragon(3))
#' rotate_dragon(d, "left")
rotate_dragon <- function(coordinates, direction, times = 1) {
    for (i in seq_len(times)) {
        if (direction %in% c("right", "clockwise")) {
            x <- coordinates[, "y"]
            y <- -coordinates[, "x"]
        } else if (direction %in% c("left", "counterclockwise")) {
            x <- -coordinates[, "y"]
            y <- coordinates[, "x"]
        }
        coordinates <- cbind(x, y)
    }
    coordinates
}
