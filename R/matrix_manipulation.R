rotate_matrix <- function(m, direction, times) {
  stopifnot(exprs = {
    class(m) == "matrix"
    direction %in% c("left", "right", "clockwise", "counterclockwise")
    times > 0
  })
  for (i in seq_len(times)) {
    if (direction %in% c("right", "clockwise")) {
      m <- t(m[nrow(m):1, ])
    } else {
      m <- t(m[, ncol(m):1])
    }
  }
  m
}


flip_matrix <- function(m, direction) {
  stopifnot(exprs = {
    class(m) == "matrix"
    direction %in% c("horizontal", "vertical")
  })
  if (direction == "horizontal") {
    m <- m[nrow(m):1, ]
  } else {
    m <- m[, ncol(m):1]
  }
  m
}

