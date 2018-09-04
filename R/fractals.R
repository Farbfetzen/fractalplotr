nice_gradients <- list(
    dark_rainbow = c("black", "red", "yellow", "green", "blue", "white"),
    black_cyan = c("black", "cyan"),
    green_red = c(rgb(0, 0.5, 0), rgb(1, 0.5, 0)),
    green_white = c(rgb(0, 0.5, 0), rgb(0.5, 0.75, 0.5), rgb(1, 1, 1)),
    white_black = c("White", "black"),
    rainbow = c("red", "orange", "yellow", "green", "cyan", "blue", "magenta"),
    rainbow_bw = c("black", "red", "orange", "yellow", "green",
                   "cyan", "blue", "magenta", "white"),
    rainbow_bb = c("black", "red", "orange", "yellow", "green",
                   "cyan", "blue", "magenta", "black"),
    bluelightblack = c(rgb(0, 0, 0.5), "white", rgb(1, 0.75, 0),
                       "darkred", "black"),
    redlightblack = c(rgb(0.5, 0, 0), "white", rgb(0.75, 0.5, 0),
                      rgb(0.25, 0.25, 1), "black")
)


make_complex_plane <- function(width, height, re_lim, im_lim) {
    x <- rep(seq(re_lim[1], re_lim[2], length.out=width), height)
    y <- rep(seq(im_lim[2], im_lim[1], length.out=height), each=width)
    complex_plane <- matrix(x + y, nrow=height, byrow=T)
    return(complex_plane)
}


save_as_image <- function(fractal_set, file_name, color_fun) {
    # Saves the fractal as a .png-image.
    #
    # Args:
    #   fractal_set: The matrix containing the fractal.
    #   file_name: The desired filename.
    #   color_fun: A function that gives the image color, e.g. the function
    #              returned from colorRampPalette.
    # Returns:
    #   Nothing.

    width <- ncol(fractal_set)
    height <- nrow(fractal_set)

    # Transpose and mirror the fractal_set because the image function draws
    # in a weird way:
    fractal_set <- t(apply(fractal_set, 2, rev))

    # Map colors to the numbers from fractal_set:
    color_palette <- color_fun(max(fractal_set) + 1)

    if (!endsWith(file_name, ".png")) file_name <- paste0(file_name, ".png")

    png(filename=file_name,
        width=width,
        height=height)
    opar <- par(no.readonly=T)
    on.exit(par(opar))
    par(mar = c(0, 0, 0, 0))
    image(fractal_set,
          col=color_palette,
          axes=F,
          ann=F,
          useRaster=T)
    dev.off()
}


save_as_csv <- function(fractal_set, file_name) {
    write.table(fractal_set, file=file_name, sep=",", col.names=F, row.names=F)
}


load_from_file <- function(file_name) {
    return(as.matrix(read.table(file_name, sep=",")))
}


adjust_im_lim <- function(width, height, re_lim, im_lim) {
    # Adjust im_lim to preserve the correct aspect ratio and prevent distortion.
    re_units_per_pixel <- abs(diff(re_lim)) / width
    im_radius <- complex(real=0,
                         imaginary=re_units_per_pixel * height / 2)
    im_center <- mean(im_lim)
    im_lim <- c(im_center - im_radius, im_center + im_radius)
    return(im_lim)
}


adjust_re_lim <- function(width, height, re_lim, im_lim) {
    # Adjust re_lim to preserve the correct aspect ratio and prevent distortion.
    im_units_per_pixel <- abs(diff(im_lim)) / height
    re_radius <- im_units_per_pixel * width / 2
    re_center <- mean(re_lim)
    re_lim <- c(re_center - re_radius, re_center + re_radius)
    return(re_lim)
}


adjust_both <- function(width, height, re_lim, im_lim) {
    # Adjust re_lim and im_lim at the same time to preserve the correct aspect
    # ratio and prevent distortion.
    re_units_per_pixel <- abs(diff(re_lim)) / width
    im_units_per_pixel <- abs(diff(im_lim)) / height
    mean_units_per_pixel <- mean(c(re_units_per_pixel, im_units_per_pixel))

    im_radius <- complex(real=0,
                         imaginary=mean_units_per_pixel * height / 2)
    im_center <- mean(im_lim)
    im_lim <- c(im_center - im_radius, im_center + im_radius)

    re_radius <- mean_units_per_pixel * width / 2
    re_center <- mean(re_lim)
    re_lim <- c(re_center - re_radius, re_center + re_radius)

    return(list(re_lim=re_lim, im_lim=im_lim))
}


fractal_jm <- function(fractal_type,
                       width,
                       height,
                       re_lim=NULL,
                       im_lim=NULL,
                       center=NULL,
                       re_width=NULL,
                       im_height=NULL,
                       max_iterations=128,
                       julia_constant=NULL,
                       adjust="none") {
    # Creates either a Julia set or a Mandelbrot set.
    #
    # Args:
    #   fractal_type: The type of fractal, either (an abbreviation of) "julia"
    #                 of "mandelbrot".
    #   width, height: The dimensions of the returned matrix in units. If the
    #                  set is to be plotted then this the image size in pixels.
    #   re_lim: The real limits of the complex plane (left and right border of
    #           the matrix). Must be a 2d numeric vector. Default is
    #           c(-1.5, 1.5) for Julia and c(-2, 1) for Mandelbrot.
    #   im_lim  The imaginary limits of the complex plane (bottom and top
    #           border of the matrix). Must be given as a 2d complex vector.
    #           Default is c(-1.5i, 1.5i) for Julia and c(-1.2i, 1.2i) for
    #           Mandelbrot.
    #   center: The center of the complex plane given as a complex number.
    #   re_width, im_height: The width and height of the complex plane.
    #   max_iterations: The maximum number of iterations each point of the
    #                   complex plane goes through. Default is 128.
    #   julia_constant: The julia constant used to calculate the Julia set.
    #                   Must be a complex number.
    #   adjust: Adjust the matrix limits to preserve the correct aspect ratio
    #           according to the given width and height to prevent distortion.
    #           Possible values are (an abbreviation of) one of these strings:
    #             "real" adjust only the left and right limits,
    #             "imaginary" adjust only the bottom and top limits,
    #             "both" adjust all limits symmetrically,
    #             "none" (the default) don't adjust the limits).
    #
    # Returns:
    #   A matrix containing the set according to the given parameters.
    #
    # The dimensions of the complex plane can be given either by specifying
    # re_lim and im_lim or by specifying center, re_width and im_height.
    # Re_lim and im_lim take precedence over the other three arguments. This
    # means if you enter re_lim, center and re_width then only re_lim
    # is used.

    if (startsWith("julia", fractal_type)) {
        fractal_type <- "julia"
        if (!is.complex(julia_constant)) {
            stop("Complex julia_constant needed for julia set.")
        }
    } else if (startsWith("mandelbrot", fractal_type)) {
        fractal_type <- "mandelbrot"
    } else {
        stop("Unknown fractal name.")
    }

    standard_limits <- list(
        julia = list(
            re_lim = c(-1.5, 1.5),
            im_lim = c(-1.5i, 1.5i)
        ),
        mandelbrot = list(
            re_lim = c(-2, 1),
            im_lim = c(-1.2i, 1.2i)
        )
    )

    if (is.null(re_lim)) {
        if (!is.null(center)) {
            if (is.null(re_width)) {
                re_width <- diff(standard_limits[[fractal_type]][["re_lim"]])
                re_lim <- c(center[1] - re_width/2, center[1] + re_width/2)
            } else {
                re_lim <- c(center[1] - re_width/2, center[1] + re_width/2)
            }
        } else {
            if (!is.null(re_width)) {
                re_center <- mean(standard_limits[[fractal_type]][["re_lim"]])
                re_lim <- c(re_center - re_width/2, re_center + re_width/2)
            } else {
                print("Using standard values for re_lim.")
                re_lim <- standard_limits[[fractal_type]][["re_lim"]]
            }
        }
    }
    if (is.null(im_lim)) {
        if (!is.null(center)) {
            if (is.null(im_height)) {
                im_height <- diff(standard_limits[[fractal_type]][["im_lim"]])
                im_lim <- c(center[2] - im_height/2, center[2] + im_height/2)
            } else {
                im_lim <- c(center[2] - im_height/2, center[2] + im_height/2)
            }
        } else {
            if (!is.null(im_height)) {
                im_center <- mean(standard_limits[[fractal_type]][["im_lim"]])
                im_lim <- c(im_center - im_height/2, im_center + im_height/2)
            } else {
                print("Using standard values for im_lim.")
                im_lim <- standard_limits[[fractal_type]][["im_lim"]]
            }
        }
    }

    if (max_iterations < 1) {
        stop("max_iterations must be a positive integer.")
    }

    if (!startsWith("none", adjust)) {
        if (startsWith("real", adjust)) {
            re_lim <- adjust_re_lim(width, height, re_lim, im_lim)
        } else if (startsWith("imaginary", adjust)) {
            im_lim <- adjust_im_lim(width, height, re_lim, im_lim)
        } else if(startsWith("both", adjust)) {
            new_limits <- adjust_both(width, height, re_lim, im_lim)
            re_lim <- new_limits$re_lim
            im_lim <- new_limits$im_lim
        }
    }

    complex_plane <- make_complex_plane(width, height, re_lim, im_lim)

    threshold <- 2
    num_iterations <- matrix(0,
                             nrow=nrow(complex_plane),
                             ncol=ncol(complex_plane))

    if (fractal_type == "julia") {
        z <- complex_plane
        to_compute <- num_iterations < max_iterations & abs(z) < threshold
        while (any(to_compute)) {
            z[to_compute] <- z[to_compute] ^ 2 + julia_constant
            num_iterations[to_compute] <- num_iterations[to_compute] + 1
            to_compute <- num_iterations < max_iterations & abs(z) < threshold
        }
    } else if (fractal_type == "mandelbrot") {
        z <- num_iterations
        to_compute <- num_iterations < max_iterations & abs(z) < threshold
        while (any(to_compute)) {
            z[to_compute] <- z[to_compute] ^ 2 + complex_plane[to_compute]
            num_iterations[to_compute] <- num_iterations[to_compute] + 1
            to_compute <- num_iterations < max_iterations & abs(z) < threshold
        }
    }

    return(num_iterations)
}
