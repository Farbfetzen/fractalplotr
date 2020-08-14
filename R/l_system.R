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
# - Optional: For every line save the current angle, length, and stack depth and
#   return those. Maybe useful for plotting (col, lwd, etc.). Color tree
#   branches differently than leaves.
# - Combine these two functions into one.


#' L-system
#'
#' Generate L-systems.
#'
#' List of valid instructions for converting the string into lines:
#' \describe{
#'   \item{`F`}{Draw a line in the current direction.}
#'   \item{`+` or `-`}{Turn by angle.}
#'   \item{`[` or `]`}{Save or load current state.}
#'   \item{`@`}{Multiply line length by following numerical argument.}
#'   \item{`!`}{Flip the angle direction.}
#' }
#'
#' @param axiom A string of symbols representing the initial state of the
#'   system.
#' @param rules A list of named strings forming the rules.
#' @param n The number of iterations.
#' @param angle The angle in radians which determines the change in direction
#'   for every "+" or "-".
#' @param initial_angle The initial angle of the first line in radians.
#' @param draw_f A character vector of symbols the replace with "F" in the
#'   instructions.
#' @param return_string Logical, defaults to `FALSE`. If `TRUE` the function
#'   returns the string of instructions after `n` iterations. Otherwise they are
#'   converted to line segments.
#'
#' @return Depending on the value of `return_string` either the string of
#'   instructions after `n` iterations or a data frame of class "l_system" with
#'   the columns x0, y0, x1, and y1 determining the endpoints of the line
#'   segments.
#'
#' @seealso [plot.l_system()]
#'
#' @examples
#' # plant:
#' l_plant <- l_system(
#'     axiom = "X",
#'     rules = list(
#'         `X` = "F+[[X]-X]-F[-FX]+X",
#'         `F` = "FF"
#'     ),
#'     n = 7,
#'     angle = pi * 0.15,
#'     initial_angle = pi * 0.45
#' )
#' plot(l_plant, col = colorRampPalette(c("#008000", "#00FF00"))(100))
#'
#' # dragon curve:
#' l_dragon <- l_system(
#'     axiom = "FX",
#'     rules = list(
#'         `X` = "X+YF+",
#'         `Y` = "-FX-Y"
#'     ),
#'     n = 12,
#'     angle = pi / 2,
#'     initial_angle = 0
#' )
#' plot(l_dragon, col = rainbow(nrow(l_dragon)))
#'
#' # sierpinski triangle:
#' l_triangle <- l_system(
#'     axiom = "F-G-G",
#'     rules = list(
#'         `F` = "F-G+F+G-F",
#'         `G` = "GG"
#'     ),
#'     n = 6,
#'     angle = 2 * pi / 3,
#'     initial_angle = pi / 3,
#'     draw_f = "G"
#' )
#' plot(l_triangle)
#'
#' # changing line length and flipping angle:ex
#' l_tree <- l_system(
#'     axiom = "X",
#'     rules = list(`X` = "F[+@.7X]F![-@.6X]F"),
#'     n = 9,
#'     angle = pi * 0.125
#' )
#' plot(l_tree)
#'
#' @export
l_system <- function(axiom,
                     rules,
                     n = 1,
                     angle,
                     initial_angle = pi / 2,
                     draw_f = NULL,
                     return_string = FALSE) {
    rule_chars <- names(rules)
    for (i in seq_len(n)) {
        new <- axiom <- strsplit(axiom, "")[[1]]
        for (char in rule_chars) {
            new[which(axiom == char)] <- rules[[char]]
        }
        axiom <- paste0(new, collapse = "")
    }
    if (return_string) return(axiom)

    for (char in draw_f) {
        axiom <- gsub(char, "F", axiom, fixed = TRUE)
    }
    instructions <- strsplit(axiom, "")[[1]]
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

    # These save state stacks are initialized with length 0 because determining
    # the maximum necessary stack size would mean looking at all instructions
    # twice, which is not worth it.
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
    class(result) <- c("l_system", class(result))
    result[!duplicated(result), ]
}


#' Plot L-systems
#'
#' Plot L-systems as line segments.
#'
#' @param x A data frame of class "l_system" as returned from
#'   [l_system()] with the columns x0, y0, x1, and y1.
#' @param ... Other parameters passed on to [graphics::segments()].
#'
#' @return None
#'
#' @examples
#' L <- l_system(
#'     "X",
#'     list(`X` = "[@.7071-FX][@.7071+FX]"),
#'     n = 10,
#'     angle = pi * 0.2
#' )
#' plot(L)
#'
#' @export
plot.l_system <- function(x, ...) {
    plot(
        NA,
        xlim = range(x$x0, x$x1),
        ylim = range(x$y0, x$y1),
        asp = 1,
        xaxs = "i",
        yaxs = "i",
        axes = FALSE,
        ann = FALSE
    )
    segments(x$x0, x$y0, x$x1, x$y1, ...)
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
