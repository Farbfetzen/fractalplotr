
# TODO:
# - Make proper tests and remove the ones here.
# - Convert the coordinate matrix into a pixel matrix where 0 is empty space
#   and 1 is the curveso it can be plottet like the other fractals.
# - Write a function for plotting the dragon with nice defaults.


make_turns <- function(order, limit = 20) {
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


# # These should generate errors:
# make_turns(c(2, 3))
# make_turns("3")
# make_turns(-1)
# make_turns(0)
# make_turns(100)
#
# # These should work:
# make_turns(1)
# make_turns(2)
# make_turns(3)
# make_turns(4)


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
  coordinates
}



# # plots:
# opar <- par(no.readonly = TRUE)
#
# par(mfrow = c(4, 4), mar = c(0, 0, 0, 0))
# for (i in 1:16) {
#   plot(get_path_coordinates(make_turns(i)), type = "l", asp = 1, axes = FALSE,
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
# coords <- get_path_coordinates(make_turns(i))
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
