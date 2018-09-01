
# https://en.wikipedia.org/wiki/Dragon_curve
# TODO:
# - Make tests
# - use the turns vector to create a matrix where 0 is empty space and 1 is the curve
# - plot it


generate_turns <- function(order, limit = 20) {
  # Note: The length of the curve will be 2 ^ order - 1.
  stopifnot(exprs = {
    is.numeric(order)
    length(order) == 1
    order >= 0
  })
  if (order > limit) {
    stop("The order exceeds the limit and the resulting vector would probably be very big in ",
         "memory. This is because the length is 2 ^ order - 1. Change the value of \"limit\" ",
         "to get higher order dragon curves.")
  }
  if (order == 0) return(numeric())
  curve <- 1
  if (order == 1) return(curve)
  for (i in 2:order) {
    middle_index <- ceiling(length(curve) / 2)
    curve_2 <- curve
    curve_2[middle_index] <- -1
    curve <- c(curve, 1, curve_2)
  }
  return(curve)
}


# These should generate return errors:
generate_turns(c(2, 3))
generate_turns("3")
generate_turns(-1)
generate_turns(100)

# These should work:
generate_turns(0)
generate_turns(1)
generate_turns(2)
generate_turns(3)
generate_turns(4)
