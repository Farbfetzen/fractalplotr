# fractalplotr (development version)

* Rewrote documentation.
* Replaced `plot_l_system()` with `plot.l_system()` so it is more consistent with the plotting methods of the other fractals.
* Renamed parameter `order` to `n` in `dragon_curve()` and also allowed a value of 1.
* Reversed meaning of argument `n` in `rotate()` and made it a required argument. Now a negative value rotates clockwise and a positive rotates counterclockwise. This is more consistent with the angles in a unit circle.
* Renamed `rotate.default()` to `rotate.matrix()` to make it more generally applicable for matrices and prevent unintended usage with wrong classes.
* Condensed the functions `grow_l_system()` and `convert_l_system()` into `l_system()`.


# fractalplotr (0.3.0)

* Added L-systems.


# fractalplotr (0.2.0)

* Added a `NEWS.md` file to track changes to the package.
