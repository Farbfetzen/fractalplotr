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


# This script is for scribbels, notes, playing around with ideas.


# w <- 1500
# h <- 1000
# threshold <- 100
# max_iterations <- 128
#
# system.time({
#   m <- mandelbrot(w, h, threshold = threshold, color_mode = "continuous")
# })
# # user  system elapsed
# # 7.79    0.86    8.75
#
# plot_mandelbrot(m)
#
# system.time({
#   res <- mandelbrot(w, h, threshold = threshold, color_mode = "continuous")
# })
#
# color_function <- colorRampPalette(c(
#   "navy", "white", "dark orange", "darkred", "black"
# ))
#
# system.time({
#   m_c <- mandelbrot_color_continuous(color_function, n_steps = res$n_steps,
#                                      z = res$z, max_iterations = max_iterations)
# })
# # user  system elapsed
# # 5.02    0.14    5.17
#
# # user  system elapsed
# # 3.53    0.11    3.66
#
#
# plot_mandelbrot(m_c)
#
# foo <- matrix(1:4, nrow = 2, byrow = TRUE)
# foo
# foo < 3
# bar <- matrix(c(1, 3, 2, 5), nrow = 2, byrow = TRUE)
# bar
# pmin(foo, bar)
# pmax(foo, bar)
