

# TODO: Make just one method for rotation. Use different methods
# for dragon curve and color matrix. Remember to fix the tests.
# TODO: Rename this script to rotate.
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
    m
}


