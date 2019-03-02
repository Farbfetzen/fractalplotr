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


rotate_matrix <- function(m, direction, times = 1) {
    for (i in seq_len(times)) {
        if (direction %in% c("r", "right", "clockwise")) {
            m <- t(m[nrow(m):1, ])
        } else if (direction %in% c("l", "left", "counterclockwise")) {
            m <- t(m[, ncol(m):1])
        } else {
            stop("direction must be one of 'r', 'right', 'clockwise', ",
                 "'l', 'left', 'anticlockwise'.")
        }
    }
    m
}


flip_matrix <- function(m, direction) {
    # TODO: Maybe make an alias 'rotate_matrix' if the function is exported.
    if (direction %in% c("v", "vertical")) {
        m <- m[nrow(m):1, ]
    } else if (direction %in% c("h", "horizontal")) {
        m <- m[, ncol(m):1]
    } else {
        stop("direction must be one of 'v', 'vertical', 'h', 'horizontal'.")
    }
    m
}
