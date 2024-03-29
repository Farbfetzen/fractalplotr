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


# TODO: Explain in the documentation how to specify the colors and also
# mention the default greyscale.
# length(color_palette) == max_iterations must be TRUE or the palette will
# be recycled with a warning.
# TODO: Define some nice color palettes as functions with
#   colorRampPalette() or colorRamp() in a separate script. Show how tu use
#   them with mandelbrot and sandpile in the examples of mandelbrot and
#   sandpile.
# TODO: Change how the color palette argument words. If it is NULL, then return
# not the colors but the raw numbers. Remove the return_colors argument. This
# makes it behave like sandpile().
# TODO: Make the colors easier and clearer to use. It's a bit complicated at the
# moment.
# TODO: Explain how specifying the coordinates works in the details section. For
# example that only two ' of the tree arguments re_width, im_height and center
# are allowed.
# TODO: Explain what the color modes are. Improve the smooth coloring so no
# lines between colors are visible if possible.


#' Mandelbrot set
#'
#' Generate the mandelbrot set.
#'
#' @param width,height The width and height in pixels.
#' @param re_width,im_height The width and height of the complex plane
#' @param center A complex number giving the center of the complex plane.
#'   Defaults to -0.5+0i.
#' @param max_iterations The maximum number of iterations. Defaults is 128
#' @param threshold The threshold value. Default is 2.
#' @param return_colors Logical. Should the colors or the number of steps be
#'   returned?
#' @param color_palette A vector of colors. Should be the same length as
#'   max_iterations.
#' @param color_inside The color of the area inside the set.
#' @param color_mode How the colors of the set will be calculated. One of
#'   "simple", "histogram" or "smooth". Can be abbreviated.
#'
#' @return A matrix of class "color_matrix" specifying the color for each pixel.
#'
#' @seealso [plot.color_matrix()]
#'
#' @examples
#' m <- mandelbrot(width = 200, height = 150)
#' plot(m)
#'
#' @export
mandelbrot <- function(width,
                       height,
                       re_width = 3.5,
                       im_height = NA,
                       center = complex(real = -0.5, imaginary = 0),
                       max_iterations = 128,
                       threshold = 2,
                       return_colors = TRUE,
                       color_palette = NULL,
                       color_inside = "black",
                       color_mode = c("simple", "histogram", "smooth")) {
    color_mode <- match.arg(color_mode)
    complex_plane <- make_complex_plane(width, height,
                                        re_width, im_height,
                                        center)
    result <- mandelbrot_iterate(complex_plane, max_iterations, threshold)
    if (!return_colors) {
        return(result$n_steps)
    }
    if (is.null(color_palette)) {
        color_palette <- grey.colors(max_iterations)
    }
    color_matrix <- switch(color_mode,
        simple = mandelbrot_color_discrete(
            result$n_steps, color_palette, color_inside, max_iterations
        ),
        histogram = mandelbrot_color_histogram(
            result$n_steps, color_palette, color_inside, max_iterations
        ),
        smooth = mandelbrot_color_smooth(
            result$n_steps, result$z, color_palette,
            color_inside, max_iterations
        )
    )
    class(color_matrix) <- c("color_matrix", class(color_matrix))
    color_matrix
}


mandelbrot_iterate <- function(complex_plane, max_iterations, threshold) {
    n_steps <- z <- matrix(
        0L,
        nrow = nrow(complex_plane),
        ncol = ncol(complex_plane)
    )

    # Check which points are inside the cardioid:
    x <- Re(complex_plane)
    y <- Im(complex_plane)
    q <- (x - 0.25)^2 + y^2
    inside <- q * (q + (x - 0.25)) <= 0.25 * y^2
    n_steps[inside] <- as.integer(max_iterations)
    todo <- !inside

    # TODO: Also check the period-2 bulb:
    # (x + 1)^2 + y^2 <= 0.0625

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


mandelbrot_color_discrete <- function(n_steps, color_palette,
                                      color_inside, max_iterations) {
    color_matrix <- matrix(color_palette[n_steps], nrow = nrow(n_steps))
    color_matrix[n_steps == max_iterations] <- color_inside
    color_matrix
}


mandelbrot_color_smooth <- function(n_steps,
                                    z,
                                    color_palette,
                                    color_inside,
                                    max_iterations) {
    # Smooth colouring, modified from pseudocode taken from
    # https://en.wikipedia.org/wiki/Mandelbrot_set#Continuous_(smooth)_coloring
    # Don't ask me how it works.
    # Needs a high threshold to produce results with invisible steps.

    color_matrix <- matrix("", nrow = nrow(n_steps), ncol = ncol(n_steps))
    outside <- n_steps < max_iterations
    color_matrix[!outside] <- color_inside
    n_steps_outside <- n_steps[outside] + 1 - log(log(abs(z[outside]), 2), 2)
    color_palette <- t(col2rgb(color_palette))
    color_1 <- color_palette[floor(n_steps_outside), ]
    color_2 <- color_palette[ceiling(n_steps_outside), ]
    d <- n_steps_outside %% 1  # fractional part
    color_min <- pmin(color_1, color_2)
    color_max <- pmax(color_1, color_2)
    color_ip <- round(color_min + (color_max - color_min) * d)
    color_ip <- format.hexmode(color_ip, width = 2)
    color_ip <- paste0("#", color_ip[, 1], color_ip[, 2], color_ip[, 3])
    color_matrix[outside] <- color_ip
    color_matrix
}


mandelbrot_color_histogram <- function(n_steps, color_palette,
                                       color_inside, max_iterations) {
    # Will distribute colors based on how many pixels reached each value.
    # Adapted from pseudocode found on Wikipedia.
    color_matrix <- matrix("", nrow = nrow(n_steps), ncol = ncol(n_steps))
    outside <- n_steps < max_iterations
    color_matrix[!outside] <- color_inside
    histogram <- tabulate(n_steps[outside], nbins = max_iterations)
    n <- sum(outside)
    hues <- numeric(n)
    cs_histo <- cumsum(histogram)
    hues <- cs_histo[n_steps[outside]]
    hues <- round(hues / n * max_iterations)
    hues <- pmin(max_iterations, pmax(1, hues))
    color_matrix[outside] <- color_palette[hues]
    color_matrix
}
