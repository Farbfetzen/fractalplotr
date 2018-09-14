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


make_complex_plane <- function(width, height, re_min, re_max, im_min, im_max) {
  x <- rep(seq(re_min, re_max, length.out = width), height)
  y <- rep(seq(im_max, im_min, length.out = height), each = width)
  matrix(x + y, nrow = height, byrow = TRUE)
}


