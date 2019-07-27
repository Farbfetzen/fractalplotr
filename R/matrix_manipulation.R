

# TODO: Make just one method for rotation. Maybe use different methods
# for dragon curve and color matrix.
# TODO: Put this into a separate script
# TODO: Export and document.
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
    invisible(m)
}


# TODO: Put this into a separate script
# TODO: Export and document.
flip <- function(m, direction) {
    cls <- class(m)
    if (startsWith("vertical", direction)) {
        m <- m[nrow(m):1, ]
    } else if (startsWith("horizontal", direction)) {
        m <- m[, ncol(m):1]
    } else {
        stop("direction must be either 'horizontal' or 'vertical' ",
             "or an abbreviation of it.")
    }
    class(m) <- cls
    invisible(m)
}
