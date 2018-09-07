
#' Fold the dragon curve
#'
#' \code{fold_dragon} creates a vector which indicates in which direction each
#' corner of the dragon curve turns.
#'
#' This function is supposed to be used in combination with
#' \code{\link{get_path_coordinates}} for which it creates the input.
#'
#' @param order The order of the dragon curve. Must be a positive
#'   integer <= \code{limit}.
#' @param limit The maximum allowed value for \code{order}. This is to prevent
#'   accidental creation of very big results which can consume cpu and memory.
#'   Defaults to 20. Be aware that the resulting vector will have a length of
#'   2^\code{order}-1 and the coordinate matrix returned by
#'   \code{get_path_coordinates} will be twice that object size.
#'
#' @return A numeric vector of length 2^\code{order}-1. The values are either
#'   1 for left turns or -1 for right turns.
#'
#' @export
#'
#' @family dragon curve functions
#'
#' @examples
#' fold_dragon(1)
#' fold_dragon(3)
#' fold_dragon(5)
fold_dragon <- function(order, limit = 20) {
  # Note: The length of the curve will be 2 ^ order - 1.
  stopifnot(exprs = {
    is.numeric(order)
    length(order) == 1
    order > 0
  })
  if (order > limit) {
    stop("The order exceeds the limit and the resulting vector would ",
         "probably be very big in memory. This is because the length is ",
         "2 ^ order - 1. Change the value of \"limit\" to get higher order ","
         dragon curves.", call. = FALSE)
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


#' Calculate the coordinates for the corners of the dragon curve
#'
#' \code{get_path_coordinates} accepts a vector of turn directions and returns
#' the coordinates of the folds in the associated curve.
#'
#' This function is supposed to be used in combination with
#' \code{\link{fold_dragon}}. Of course, it can also return a path for other
#' vectors of turns if they conform to the input format.
#'
#' @param turns A numeric vector consisting solely of the values 1 and -1. A 1
#'   results in a left turn and a -1 makes a right turn.
#'
#' @return A numeric matrix with two columns x and y containing the coordinates
#'   of the folds in the dragon curve.
#'
#' @export
#'
#' @family dragon curve functions
#'
#' @examples
#' folds <- fold_dragon(3)
#' get_path_coordinates(folds)
#'
#' # The matrix can be used easily plot the dragon curve:
#' folds <- fold_dragon(8)
#' coords <- get_path_coordinates(folds)
#' plot(coords, type = "l", asp = 1)
get_path_coordinates <- function(turns) {
  stopifnot(exprs = {
    is.numeric(turns)
    length(turns) > 0
    all(turns %in% c(-1, 1))
  })
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
  apply(coordinates, 2, function(column) column - min(column))
}



# notes and stuff, remove later -------------------------------------------

# TODO:
# - Decide if I want to call it "turns" or "folds" or a mix of both. Either
#   way the nomenclature should be consistent.
# - Make proper tests and remove the ones here.
# - Convert the coordinate matrix into a pixel matrix where 0 is empty space
#   and 1 is the curveso it can be plottet like the other fractals.
# - Write a function for plotting the dragon with nice defaults.


# # These should generate errors:
# fold_dragon(c(2, 3))
# fold_dragon("3")
# fold_dragon(-1)
# fold_dragon(0)
# fold_dragon(100)
#
# # These should work:
# fold_dragon(1)
# fold_dragon(2)
# fold_dragon(3)
# fold_dragon(4)


# # plots:
# opar <- par(no.readonly = TRUE)
#
# par(mfrow = c(4, 4), mar = c(0, 0, 0, 0))
# for (i in 1:16) {
#   plot(get_path_coordinates(fold_dragon(i)), type = "l", asp = 1, axes = FALSE,
#        ann = FALSE)
#   legend("topleft", legend = i, bty = "n")
#   box()
# }
# par(opar)
#
# # I use segments here because it allows lines to be colored differently. lines
# # only does one color for the whole curve.
#
# i <- 16
# coords <- get_path_coordinates(fold_dragon(i))
# xrange <- range(coords[, "x"])
# yrange <- range(coords[, "y"])
#
# #png(paste0("dragon ", i, ".png"), width = diff(xrange) * 2,
# #    height = diff(yrange) * 2, res = 96)
# par(mar = c(0, 0, 0, 0))
# plot(NA, NA, xlim = xrange, ylim = yrange,
#     asp = 1, axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
# colors <- rainbow(nrow(coords) - 1)
# segments(coords[-nrow(coords), "x"], coords[-nrow(coords), "y"],
#          coords[-1, "x"], coords[-1, "y"], col = colors)
# # dev.off()
#
# # alternative coloring:
# # png(paste0("dragon ", i, " alt_coloring.png"), width = diff(xrange) * 2,
# #     height = diff(yrange) * 2, res = 96)
# par(mar = c(0, 0, 0, 0))
# plot(NA, NA, xlim = xrange, ylim = yrange,
#      asp = 1, axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
# colors <- rep(rainbow(i), c(2, 2^(1:(16-1))))
# segments(coords[-nrow(coords), "x"], coords[-nrow(coords), "y"],
#          coords[-1, "x"], coords[-1, "y"], col = colors)
# # dev.off()
