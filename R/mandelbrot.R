# TODO: Explain in the documentation how to specify the colors and also
# mention the default greyscale.
# length(color_palette) == max_iterations must be TRUE or the palette will
# be recycled with a warning.


#' Title
#'
#' TODO: fixme
#'
#' @param width foo
#' @param height bar
#' @param re_width baz
#' @param im_height hurr
#' @param center blubb
#' @param max_iterations hurz
#' @param threshold a
#' @param return_colors b
#' @param color_palette c
#' @param color_inside d
#' @param color_mode e
#'
#' @return waaa color_matrix
#' @export
#'
#' @examples
#' mandelbrot(width = 10, height = 5)
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
                       color_mode = "simple") {
    complex_plane <- make_complex_plane(width, height,
                                        re_width, im_height,
                                        center)
    result <- mandelbrot_iterate(complex_plane, max_iterations, threshold)
    if (!return_colors) {
        return(invisible(result$n_steps))
    }
    if (is.null(color_palette)) {
        color_palette <- grey.colors(max_iterations)
    }
    if (color_mode == "simple") {
        color_matrix <- mandelbrot_color_discrete(
            result$n_steps, color_palette, color_inside, max_iterations
        )
    } else if (color_mode == "histogram") {
        color_matrix <- mandelbrot_color_histogram(
            result$n_steps, color_palette, color_inside, max_iterations
        )
    } else if (color_mode == "smooth") {
        color_matrix <- mandelbrot_color_smooth(
            result$n_steps, result$z, color_palette,
            color_inside, max_iterations
        )
    } else {
        stop("Unknown color_mode. Please use 'simple', 'histogram' or ",
             "'smooth'.", call. = FALSE)
    }
    class(color_matrix) <- c("color_matrix", class(color_matrix))
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
