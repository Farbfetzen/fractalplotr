context("mandelbrot set")


test_that("mandelbrot set is calculated correctly", {
  filepath <- system.file("testdata", "mandelbrot.csv",
                          package = "fractalplotr", mustWork = TRUE)
  m_reference <- as.matrix(read.table(filepath, sep = ","))
  dimnames(m_reference) <- NULL
  m <- mandelbrot(width = 150, height = 100, re_min = -2, re_max = 1,
                  im_min = -1i, im_max = 1i, max_iterations = 128,
                  threshold = 2)
  expect_identical(m, m_reference)
})
