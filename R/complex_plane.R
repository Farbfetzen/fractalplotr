make_complex_plane <- function(width, height, re_width, im_height, center) {
    stopifnot(exprs = {
        is.numeric(width)
        is.numeric(height)
        is.numeric(re_width) || is.na(re_width)
        is.numeric(im_height) || is.na(im_height)
        is.complex(center) || is.numeric(center)
    })

    if (is.na(re_width) & is.na(im_height)) {
        stop("You must provide either re_width or im_height or both.",
             call. = FALSE)
    } else if (is.na(re_width)) {
        re_width <- im_height / height * width
    } else if (is.na(im_height))
        im_height <- re_width / width * height

    # Convert coordinates to get the limits:
    re_min <- Re(center) - re_width / 2
    re_max <- Re(center) + re_width / 2
    im_min <- complex(imaginary = Im(center) - im_height / 2)
    im_max <- complex(imaginary = Im(center) + im_height / 2)

    x <- rep(seq(re_min, re_max, length.out = width), height)
    y <- rep(seq(im_max, im_min, length.out = height), each = width)
    matrix(x + y, nrow = height, byrow = TRUE)
}
