source("fractals.R")

julia_dir <- "/pics_julia/"
mandelbrot_dir <- "/pics_mandelbrot/"


# interesting julia constants:
# -0.221-0.713i
# -0.233-0.666i
# -0.631+0.466i
# -0.744-0.118i
# 0.366+0.366i
# -0.501-0.523i
# 0.285+0.013
# -0.857+0.206i
# -242-648i

# interesting mandelbrot coordinates:
# re_lim = c(-0.79, -0.73), im_lim = c(0.1i, 0.15i)
# re_lim = c(-0.17375, -0.14825), im_lim = c(1.023675i, 1.044075i)


test_julia <- fractal_jm("julia",
                         julia_constant=-0.857+0.206i,
                         width=1000,
                         height=800,
                         adjust="im")
save_as_image(test_julia,
              "test_julia.png",
              colorRampPalette(nice_gradients$white_black))
save_as_csv(test_julia, "test_julia.csv")
test_julia_2 <- load_from_file("test_julia.csv")
save_as_image(test_julia_2,
              "test_julia_2.png",
              colorRampPalette(nice_gradients$white_black))


# Test aspect ratio adjustments:
test_julia_ad_n <- test_julia <- fractal_jm("julia",
                                            julia_constant=-0.857+0.206i,
                                            width=500,
                                            height=400)
save_as_image(test_julia_ad_n,
              "test_julia_ad_n.png",
              colorRampPalette(nice_gradients$white_black))
test_julia_ad_im <- test_julia <- fractal_jm("julia",
                                             julia_constant=-0.857+0.206i,
                                             width=500,
                                             height=400,
                                             adjust="im")
save_as_image(test_julia_ad_im,
              "test_julia_ad_im.png",
              colorRampPalette(nice_gradients$white_black))
test_julia_ad_re <- test_julia <- fractal_jm("julia",
                                             julia_constant=-0.857+0.206i,
                                             width=500,
                                             height=400,
                                             adjust="re")
save_as_image(test_julia_ad_re,
              "test_julia_ad_re.png",
              colorRampPalette(nice_gradients$white_black))
test_julia_ad_both <- test_julia <- fractal_jm("julia",
                                               julia_constant=-0.857+0.206i,
                                               width=500,
                                               height=400,
                                               adjust="b")
save_as_image(test_julia_ad_both,
              "test_julia_ad_both.png",
              colorRampPalette(nice_gradients$white_black))



test_mandelbrot <- fractal_jm("mandelbrot",
                              width=1000,
                              height=800,
                              adjust="im")
save_as_image(test_mandelbrot,
              "test_mandelbrot.png",
              colorRampPalette(nice_gradients$white_black))

test_mandelbrot_2 <- fractal_jm("mandelbrot",
                                width=500,
                                height=400,
                                adjust="both",
                                re_lim = c(-0.17375, -0.14825),
                                im_lim = c(1.023675i, 1.044075i))
save_as_image(test_mandelbrot_2,
              "test_mandelbrot_2.png",
              colorRampPalette(nice_gradients$rainbow))


for (i in 1:10) {
    x <- 10^(-i)
    max_iter <- 100 + 400 * (i - 1)
    save_as_image(
        fractal_jm(
            "mandelbrot",
            width=500,
            height=400,
            center=c(-0.743643887037151, 0.131825904205330i),
            re_width=x,
            adjust="im",
            max_iterations=max_iter
        ),
        paste0("mandelbrot_test_", format(x, scientific=F), ".png"),
        colorRampPalette(nice_gradients$bluelightblack)
    )

}



