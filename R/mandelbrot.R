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
mandelbrot <- function(width,
                       height,
                       re_width,
                       im_height,
                       center = -0.5+0i,
                       max_iterations = 128,
                       threshold = 2,
                       color_mode = "discrete",
                       color_function = ""){
    # TODO:
    # Argument checking: Either provide re_width or im_height but never both.

    # Check arguments and get the missing dimension:
    if (missing(im_height) & !missing(re_width)) {
        im_height <- re_width / width * height
    } else if (!missing(im_height) & missing(re_width)) {
        re_width <- im_height / height * width
    } else {
        stop("Wrong combination of arguments provided. Provide either re_width",
             " or im_height but not both.")
    }

    # Convert coordinates to get the limits:
    re_min <- Re(center) - re_width / 2
    re_max <- Re(center) + re_width / 2
    im_min <- complex(imaginary = Im(center) - im_height / 2)
    im_max <- complex(imaginary = Im(center) + im_height / 2)

    complex_plane <- make_complex_plane(width, height,
                                        re_min, re_max, im_min, im_max)
    result <- mandelbrot_iterate(complex_plane, max_iterations, threshold)

    if (color_function == "") {
        color_function <- colorRampPalette(
            c("white", "black")
        )
        # TODO: Explain in the documentation how to specify the colors and
        # include this in the example:
        # color_function <- colorRampPalette(
        #     c("navy", "white", rgb(1, 0.75, 0), "darkred", "black")
        # )
    }
    if (color_mode == "discrete") {
        color_matrix <- mandelbrot_color_discrete(
            color_function, result$n_steps, max_iterations
        )
    }
    if (color_mode == "continuous") {
        color_matrix <- mandelbrot_color_continuous(
            color_function, result$n_steps, result$z, max_iterations
        )
    }

    invisible(color_matrix)
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


mandelbrot_color_discrete <- function(color_fun, n_steps, max_iterations) {
    color_palette <- color_fun(max_iterations)
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
