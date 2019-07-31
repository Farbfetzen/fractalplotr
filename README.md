
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fractalplotr

This package lets you create and plot fractals.

currently implemented:

  - [Dragon curve](https://en.wikipedia.org/wiki/Dragon_curve)
  - [Mandelbrot set](https://en.wikipedia.org/wiki/Mandelbrot_set)
  - [Sandpile](https://en.wikipedia.org/wiki/Abelian_sandpile_model)

planned:

  - [Julia set](https://en.wikipedia.org/wiki/Julia_set)
  - [Ulam-Warburton cellular
    automaton](https://en.wikipedia.org/wiki/Ulam%E2%80%93Warburton_automaton)
  - [toothpick
    pattern](https://en.wikipedia.org/wiki/Toothpick_sequence)

## Installation

You can install fractalplotr from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("bastihz/fractalplotr")
```

## Examples

**Dragon curve**

``` r
d <- dragon_curve(12)
plot(d, col = "forestgreen")
```

<img src="man/figures/README-dragon-1.png" width="100%" />

**Mandelbrot
set**

``` r
blue_to_black <- colorRampPalette(c(rgb(0, 0, 0.5), "white", rgb(1, 0.75, 0), 
                                    "darkred", "black"))
m <- mandelbrot(
    1200, 
    1200,
    re_width = 2.5,
    center = -0.75,
    color_palette = blue_to_black(128),
    color_mode = "smooth"
)
plot(m)
```

<img src="man/figures/README-mandelbrot-1.png" width="100%" />

**Sandpile**

``` r
s <- sandpile(1e5, c("white", "yellow", "orange", "red"))
plot(s)
```

<img src="man/figures/README-sandpile-1.png" width="100%" />
