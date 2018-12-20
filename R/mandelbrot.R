# fractalplotr - Plot Beautiful Fractals with R
# Copyright (C) 2018 Sebastian Henz
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
# along with this program. If not, see http://www.gnu.org/licenses.


#' @export
mandelbrot <- function(width, height, re_min = -2, re_max = 1,
                       im_min = -1i, im_max = 1i,
                       center = -0.5+0i, re_width, im_height,
                       max_iterations = 128, threshold = 2,
                       prevent_distortion = TRUE, color_mode = "discrete",
                       color_function = "") {
    # TODO:
    # - define a default color_fun and write it in the arguments
    # - check input: if prevent_distortion only some of the arguments should be
    #   given
    # mandelbrot_check_input()

    complex_plane <- make_complex_plane(width, height,
                                        re_min, re_max,
                                        im_min, im_max)
    result <- mandelbrot_iterate(complex_plane, max_iterations, threshold)

    if (color_mode == "none") {
        return(result$n_steps)
    }

    if (color_function == "") {
        color_function <- colorRampPalette(
            c("navy", "white", rgb(1, 0.75, 0), "darkred", "black")
        )
    }
    if (color_mode == "discrete") {
        return(mandelbrot_color_discrete(color_function, result$n_steps))
    }
    if (color_mode == "continuous") {
        return(mandelbrot_color_continuous(
            color_function, result$n_steps, result$z, max_iterations
        ))
    }
}


mandelbrot_check_input <- function() {

}


mandelbrot_iterate <- function(complex_plane, max_iterations, threshold) {
    n_steps <- z <- matrix(
        0L,
        nrow = nrow(complex_plane),
        ncol = ncol(complex_plane)
    )

    # Check which points are inside the cardioid or the period-2 bulb:
    x <- Re(complex_plane)
    y <- Im(complex_plane)
    q <- (x - 0.25)^2 + y^2
    inside <- q * (q + (x - 0.25)) < 0.25 * y^2
    n_steps[inside] <- as.integer(max_iterations)
    todo <- !inside

    # Note to future me: I tried to implement periodicity checking but
    # it didn't improve performance.

    for (i in seq_len(max_iterations)) {
        todo[todo] <- abs(z[todo]) < threshold
        n_steps[todo] <- n_steps[todo] + 1L
        z[todo] <- z[todo] ^ 2 + complex_plane[todo]
        if (!any(todo)) break
    }

    list(n_steps = n_steps, z = z)
}


mandelbrot_color_discrete <- function(color_fun, n_steps) {
    color_palette <- color_fun(max(n_steps))
    matrix(color_palette[n_steps], nrow = nrow(n_steps))
}


mandelbrot_color_continuous <- function(color_fun,
                                        n_steps,
                                        z,
                                        max_iterations) {
    # Smooth colouring, modified from pseudocode taken from
    # https://en.wikipedia.org/wiki/Mandelbrot_set#Continuous_(smooth)_coloring
    # Don't ask me how it works.
    # Needs a high threshold to produce results with invisible steps.

    outside <- n_steps < max_iterations
    n_steps_outside <- n_steps[outside] + 1 - log(log(abs(z[outside]), 2), 2)
    result <- matrix("", nrow = nrow(outside), ncol = ncol(outside))

    color_palette <- color_fun(max_iterations)
    result[!outside] <- tail(color_palette, 1)
    color_palette <- t(col2rgb(color_palette))
    color_1 <- color_palette[floor(n_steps_outside), ]
    color_2 <- color_palette[ceiling(n_steps_outside), ]
    d <- n_steps_outside %% 1  # fractional part

    color_min <- pmin(color_1, color_2)
    color_max <- pmax(color_1, color_2)
    color_ip <- round(color_min + (color_max - color_min) * d)
    color_ip <- format.hexmode(color_ip, width = 2)
    color_ip <- paste0("#", color_ip[, 1], color_ip[, 2], color_ip[, 3])
    result[outside] <- color_ip
    result
}


#' @export
plot_mandelbrot <- function(color_matrix, file = "") {
    if (file != "") {
        on.exit(dev.off())
        png(file, width = ncol(color_matrix), height = nrow(color_matrix))
    }
    grid::grid.raster(color_matrix, interpolate = FALSE)
}
