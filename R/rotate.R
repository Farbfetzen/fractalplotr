#' Rotate Fractals
#'
#' Rotate a fractal either clockwise or counterclockwise
#'
#' @param fractal The fractal to be rotated.
#' @param direction The direction of rotation, either "clockwise" or
#'   "counterclockwise". Can be abbreviated.
#'
#' @return The rotated fractal.
#' @export
#'
#' @examples
#' d1 <- dragon_curve(10)
#' d2 <- rotate(d, "clockwise")
#' plot(d1)
#' plot(d2)
rotate <- function(fractal, direction) {
    UseMethod("rotate")
}


rotate.dragon_curve <- function(fractal, direction) {
    stop("not yet implemented")
}


rotate.color_matrix <- function(fractal, direction) {
    stop("not yet implemented")
}


# IDEE: statt direction als string zu nehmen, einfach eine Ganzzahl akzeptieren.
# Positiv dreht im Uhrzeigersinn und negativ entgegen. Und der Betrag bestimmt,
# wie of um 90° gedreht wird. Kümmere dich nicht darum die Zahl auf 1:3 zu
# beschränken, denn so schlau sollten die User schon sein (also dass sie nicht
# 25 oder so eingeben).

# TODO: Make just one method for rotation. Use different methods
# for dragon curve and color matrix. Remember to fix the tests.
# TODO: Rename this script to rotate.
# TODO: Export and document.

# rotate_matrix <- function(m, direction, times = 1) {
#     cls <- class(m)
#     for (i in seq_len(times)) {
#         if (direction %in% c("r", "right", "clockwise")) {
#             m <- t(m[nrow(m):1, ])
#         } else if (direction %in% c("l", "left", "counterclockwise")) {
#             m <- t(m[, ncol(m):1])
#         } else {
#             stop("direction must be one of 'r', 'right', 'clockwise', ",
#                  "'l', 'left', 'anticlockwise'.")
#         }
#     }
#     class(m) <- cls
#     m
# }


