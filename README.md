
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

`fractalplotr` is not on CRAN. You can install the most recent version
from GitHub using the devtools package:

``` r
# install.packages("devtools")
devtools::install_github("bastihz/fractalplotr")
```

RStudio’s integrated package updater won’t detect updates in packages
installed from GitHub. I recommend running

``` r
devtools::update_packages()
```

in regular intervals to check for updates from those sources.

## Examples

### Dragon curve

``` r
d <- dragon_curve(12)
plot(d, col = "forestgreen")
```

![](man/figures/README-dragon-1.png)<!-- -->

### Mandelbrot set

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

![](man/figures/README-mandelbrot-1.png)<!-- -->

### Sandpile

``` r
s <- sandpile(1e5, c("white", "yellow", "orange", "red"))
plot(s)
```

![](man/figures/README-sandpile-1.png)<!-- -->
