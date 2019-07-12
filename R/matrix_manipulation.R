rotate_matrix <- function(m, direction, times = 1) {
    cls <- class(m)
    for (i in seq_len(times)) {
        if (direction %in% c("r", "right", "clockwise")) {
            m <- t(m[nrow(m):1, ])
        } else if (direction %in% c("l", "left", "counterclockwise")) {
            m <- t(m[, ncol(m):1])
        } else {
            stop("direction must be one of 'r', 'right', 'clockwise', ",
                 "'l', 'left', 'anticlockwise'.")
        }
    }
    class(m) <- cls
    m
}


flip_matrix <- function(m, direction) {
    cls <- class(m)
    if (direction %in% c("v", "vertical")) {
        m <- m[nrow(m):1, ]
    } else if (direction %in% c("h", "horizontal")) {
        m <- m[, ncol(m):1]
    } else {
        stop("direction must be one of 'v', 'vertical', 'h', 'horizontal'.")
    }
    class(m) <- cls
    m
}
