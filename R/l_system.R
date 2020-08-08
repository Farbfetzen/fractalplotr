# notes:
# - make it possible to modify the line length. see that reddit post


# Grow an L-system for n iterations
# export this function
expand_l_system <- function(axiom, rules, n = 1) {
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

# expand_l_system(axiom, rules, 1)
# expand_l_system(axiom, rules, 2)
# expand_l_system(axiom, rules, 3)



# generate the points from the instructions
# TODO: name more fitting for L-system
generate_points <- function(instructions, angle, initial_angle = pi / 2) {
    # angles: 0 = right, pi/2 = up, pi = left, pi*3/2 = down
    instructions <- strsplit(instructions, "")[[1]]
    n_lines <- sum(instructions == "F")
    if (n_lines == 0) {
        stop("Instructions do not contain any 'F'.")
    }
    x0 <- y0 <- x1 <- y1 <- numeric(n_lines)
    position <- c(0, 0)
    current_angle <- initial_angle
    line_idx <- 0
    saved_positions <- list()
    saved_angles <- numeric()
    save_idx <- 0
    tau <- 2 * pi

    for (i in seq_along(instructions)) {
        switch(
            instructions[i],
            `F` = {
                # move forward
                line_idx <- line_idx + 1
                x0[line_idx] <- position[1]
                y0[line_idx] <- position[2]
                position <- position + c(cos(current_angle), sin(current_angle))
                x1[line_idx] <- position[1]
                y1[line_idx] <- position[2]
            },
            `+` = {
                # turn counterclockwise
                current_angle <- (current_angle + angle) %% tau
            },
            `-` = {
                # turn clockwise
                current_angle <- (current_angle - angle) %% tau
            },
            `[` = {
                # save state
                save_idx <- save_idx + 1
                saved_positions[[save_idx]] <- position
                saved_angles[save_idx] <- current_angle
            },
            `]` = {
                # load state
                position <- saved_positions[[save_idx]]
                current_angle <- saved_angles[[save_idx]]
                save_idx <- save_idx - 1
            }
        )
    }
    result <- data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1)
    result[!duplicated(result), ]
}


plot_l_system <- function(points, ...) {
    plot(
        NA,
        xlim = range(p$x0, p$x1),
        ylim = range(p$y0, p$y1),
        asp = 1,
        axes = FALSE,
        ann = FALSE
    )
    segments(p$x0, p$y0, p$x1, p$y1, ...)
}



# animated:
# foo <- expand_l_system(axiom, rules, 5)
# p <- generate_points(foo, angle, 0.4 * pi)
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


# Examples from Wikipedia
# Make all of them into test cases (with fewer iterations). Compare either the
# pointlists or the images.

par(mfrow = c(1, 3))

# plant:
axiom <- "X"
rules <-  list(
    `X` = "F+[[X]-X]-F[-FX]+X",
    `F` = "FF"
)
angle <- pi * 0.15
L <- expand_l_system(axiom, rules, 7)
p <- generate_points(L, angle, pi * 0.45)
plot_l_system(p, col = "forestgreen")

# dragon curve:
axiom <- "FX"
rules <- list(
    `X` = "X+YF+",
    `Y` = "-FX-Y"
)
angle <- pi / 2
D <- expand_l_system(axiom, rules, n = 12)
p <- generate_points(D, angle)
plot_l_system(p)

# sierpinski triangle:
axiom <- "F-G-G"
rules <- list(
    `F` = "F-G+F+G-F",
    `G` = "GG"
)
angle <- 2 * pi / 3
S <- expand_l_system(axiom, rules, 6)
# Achtung: Hier heißt "G" auch "draw forward"! Daher vorher mit "F" ersetzen.
# Vielleicht zusätzliche Option in generate_points? Einen Vektor mit Symbolen,
# die durch F ersetzt werden sollen?
S <- gsub("G", "F", S, fixed = TRUE)
p <- generate_points(S, angle, angle/2)
plot_l_system(p)

