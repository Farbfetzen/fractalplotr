make_complex_plane <- function(width, height, re_min, re_max, im_min, im_max) {
  x <- rep(seq(re_min, re_max, length.out = width), height)
  y <- rep(seq(im_max, im_min, length.out = height), each = width)
  matrix(x + y, nrow = height, byrow = TRUE)
}


