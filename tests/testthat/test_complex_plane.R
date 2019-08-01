context("complex plane")


test_that("complex plane is correctly created", {
    m1 <- matrix(
        c(-1+4i, 0+4i, 1+4i, 2+4i,
          -1+3i, 0+3i, 1+3i, 2+3i,
          -1+2i, 0+2i, 1+2i, 2+2i),
        nrow = 3,
        byrow = TRUE
    )
    m2 <- make_complex_plane(4, 3, 3, 2, 0.5+3i)
    expect_identical(m1, m2)
})
