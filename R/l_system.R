# Copyright (C) 2020 Sebastian Henz
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
# along with this program.  If not, see https://www.gnu.org/licenses/.


# TODO:
# - Make it possible to modify the line length. See that reddit post.
# - Make an S3 method for plotting, same as for the other fractals.
# - Explaint all instructions in "Details" of documentation.
# - Add examples of change line length and flip angle


#' L-system
#'
#' Expand, convert and plot L-systems or Lindenmayer systems.
#'
#' @name l_system
#'
#' @param axiom A string of symbols representing the initial state of the
#'   system.
#' @param rules A list of named strings forming the rules.
#' @param n The number of iterations.
#' @param instructions A string of symbols to convert to line segments.
#' @param angle The angle in radians which determines the change in direction
#'   for every "+" or "-".
#' @param initial_angle The initial direction of the first line in radians.
#' @param draw_f A character vector of symbols the replace with "F" in the
#'   instructions.
#' @param l_lines A data frame with the columns x0, y0, x1, and y1 determining
#'   the endpoints of the line segments.
#' @param ... Other parameters passed on to
#'   \code{\link[graphics:segments]{segments}}.
#'
#' @return \code{grow_l_system} returns a string of instructions after \code{n}
#'   iterations of the system.
#'
#'   \code{convert_l_system} returns a data frame with the columns x0, y0, x1,
#'   and y1 determining the endpoints of the line segments.
#'
#'   \code{plot_l_system} returns NULL.
#'
#' @examples
#' par(mfrow = c(1, 3))
#'
#' # plant:
#' l_system <- grow_l_system(
#'     axiom = "X",
#'     rules = list(
#'         `X` = "F+[[X]-X]-F[-FX]+X",
#'         `F` = "FF"
#'     ),
#'     n = 7
#' )
#' l_lines <- convert_l_system(
#'     instructions = l_system,
#'     angle = pi * 0.15,
#'     initial_angle = pi * 0.45
#' )
#' plot_l_system(
#'     l_lines = l_lines,
#'     col = "forestgreen"
#' )
#'
#' # dragon curve:
#' l_system <- grow_l_system(
#'     axiom = "FX",
#'     rules = list(
#'         `X` = "X+YF+",
#'         `Y` = "-FX-Y"
#'     ),
#'     n = 12
#' )
#' l_lines <- convert_l_system(
#'     instructions = l_system,
#'     angle = pi / 2
#' )
#' plot_l_system(l_lines = l_lines)
#'
#' # sierpinski triangle:
#' l_system <- grow_l_system(
#'     axiom = "F-G-G",
#'     rules = list(
#'         `F` = "F-G+F+G-F",
#'         `G` = "GG"
#'     ),
#'     n = 6)
#' l_lines <- convert_l_system(
#'     instructions = l_system,
#'     angle = 2 * pi / 3,
#'     initial_angle = pi / 3,
#'     draw_f = "G"
#' )
#' plot_l_system(l_lines = l_lines)
NULL



#' @rdname l_system
#' @export
grow_l_system <- function(axiom, rules, n = 1) {
    rule_chars <- names(rules)
    for (i in seq_len(n)) {
        new <- axiom <- strsplit(axiom, "")[[1]]
        for (char in rule_chars) {
            new[which(axiom == char)] <- rules[[char]]
        }
        axiom <- paste0(new, collapse = "")
    }
    axiom
}


#' @rdname l_system
#' @export
convert_l_system <- function(instructions, angle, initial_angle = pi / 2,
                             draw_f = NULL) {
    # angles: 0 = right, pi/2 = up, pi = left, pi*3/2 = down
    for (char in draw_f) {
        instructions <- gsub(char, "F", instructions, fixed = TRUE)
    }
    instructions <- strsplit(instructions, "")[[1]]
    n_lines <- sum(instructions == "F")
    if (n_lines == 0) {
        stop("Instructions do not contain any 'F'.")
    }
    x0 <- y0 <- x1 <- y1 <- numeric(n_lines)
    position <- c(0, 0)
    current_angle <- initial_angle
    line_idx <- 0
    tau <- 2 * pi
    len_instructions <- length(instructions)
    line_length <- 1
    numerics <- strsplit(".0123456789", "")[[1]]
    save_idx <- 0
    saved_positions <- list()
    saved_angles <- numeric()
    saved_current_angles <- numeric()
    saved_line_lengths <- numeric()

    i <- 0
    while (i < len_instructions) {
        i <- i + 1
        switch(
            instructions[i],
            `F` = {
                # move forward
                line_idx <- line_idx + 1
                x0[line_idx] <- position[1]
                y0[line_idx] <- position[2]
                position <- position +
                    c(cos(current_angle), sin(current_angle)) * line_length
                x1[line_idx] <- position[1]
                y1[line_idx] <- position[2]
            },
            `+` = {
                # change line angle
                current_angle <- (current_angle + angle) %% tau
            },
            `-` = {
                # change line angle
                current_angle <- (current_angle - angle) %% tau
            },
            `[` = {
                # save state
                save_idx <- save_idx + 1
                saved_positions[[save_idx]] <- position
                saved_angles[save_idx] <- angle
                saved_current_angles[save_idx] <- current_angle
                saved_line_lengths[save_idx] <- line_length
            },
            `]` = {
                # load state
                position <- saved_positions[[save_idx]]
                angle <- saved_angles[[save_idx]]
                current_angle <- saved_current_angles[[save_idx]]
                line_length <- saved_line_lengths[save_idx]
                save_idx <- save_idx - 1
            },
            `@` = {
                # multiply line length by following number
                num <- ""
                for (j in seq(i + 1, len_instructions)) {
                    if (instructions[j] %in% numerics) {
                        num <- paste0(num, instructions[j])
                    } else {
                        break
                    }
                }
                if (num == "") {
                    stop("No numeric argument after '@'.")
                }
                line_length <- line_length * as.numeric(num)
                i <- j - 1
            },
            `!` = {
                # flip angle turn direction
                angle <- -angle
            }
        )
    }
    result <- data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1)
    result[!duplicated(result), ]
}


#' @rdname l_system
#' @export
plot_l_system <- function(l_lines, ...) {
    plot(
        NA,
        xlim = range(l_lines$x0, l_lines$x1),
        ylim = range(l_lines$y0, l_lines$y1),
        asp = 1,
        axes = FALSE,
        ann = FALSE
    )
    segments(l_lines$x0, l_lines$y0, l_lines$x1, l_lines$y1, ...)
}



# animated:
# foo <- grow_l_system(axiom, rules, 5)
# p <- convert_l_system(foo, angle, 0.4 * pi)
# plot_incremental <- function(points, n = 10, delay = 0.1, ...) {
#     plot(NA, xlim = range(p$x0, p$x1), ylim = range(p$y0, p$y1), asp = 1)
#     i <- 0
#     for (i in seq(i, nrow(points), n)) {
#         j <- seq(i, i + n - 1)
#         segments(p$x0[j], p$y0[j], p$x1[j], p$y1[j], ...)
#         Sys.sleep(delay)
#     }
# }
# plot_incremental(p, n = 10)
