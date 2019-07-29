
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fractalplotr

The goal of fractalplotr is to … (FIXME) This package is currently under
heavy construction. Nothing works and everything is on fire.

## Installation

You can install fractalplotr from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("bastihz/fractalplotr")
```

## Examples

TODO: better image resolution, maybe a little smaller images? See how it
looks on the phone and the laptop. \# FIXME: If I can’t set the size via
the chunk then I could just programmatically generate and save the
pictures and include them like any other picture.

**Dragon curves** (TODO: link to wikipedia)

``` r
d <- dragon_curve(12)
plot(d)
```

<img src="man/figures/README-dragon-1.png" width="100%" />

**Mandelbrot sets** (TODO: link to wikipedia)

``` r
m <- mandelbrot(
    800, 
    600, 
    color_palette = colorRampPalette(c("black", "green"))(128)
)
plot(m)
```

<img src="man/figures/README-mandelbrot-1.png" width="100%" />

**Sandpiles** (TODO: link to wikipedia)

``` r
s <- sandpile(1e5)
plot(s)
```

<img src="man/figures/README-sandpile-1.png" width="100%" />
