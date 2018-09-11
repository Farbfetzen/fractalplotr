context("complex plane")


test_that("complex plane is correctly created", {
  m1 <- matrix(
    c(-1+4i, 0+4i, 1+4i, 2+4i,
      -1+3i, 0+3i, 1+3i, 2+3i,
      -1+2i, 0+2i, 1+2i, 2+2i),
    nrow = 3,
    byrow = TRUE
    )
  m2 <- make_complex_plane(4, 3, -1, 2, 2i, 4i)
  expect_identical(m1, m2)
})


m <- matrix(1:9, nrow = 3, byrow = TRUE)

test_that("matrix is rotated clockwise", {
  mr1 <- t(apply(m, 2, rev))
  mr2 <- t(apply(mr1, 2, rev))
  mr3 <- t(apply(mr2, 2, rev))
  expect_identical(mr1, rotate_matrix(m, "right", 1))
  expect_identical(mr2, rotate_matrix(m, "clockwise", 2))
  expect_identical(mr3, rotate_matrix(m, "right", 3))
  expect_identical(m, rotate_matrix(m, "clockwise", 4))
})

test_that("matrix is rotated anticlockwise", {
  ml1 <- apply(t(m), 2, rev)
  ml2 <- apply(t(ml1), 2, rev)
  ml3 <- apply(t(ml2), 2, rev)
  expect_identical(ml1, rotate_matrix(m, "left", 1))
  expect_identical(ml2, rotate_matrix(m, "anticlockwise", 2))
  expect_identical(ml3, rotate_matrix(m, "left", 3))
  expect_identical(m, rotate_matrix(m, "anticlockwise", 4))
})

test_that("matrix is flipped as expected", {
  expect_identical(m[nrow(m):1, ], flip_matrix(m, "horizontal"))
  expect_identical(m[, ncol(m):1], flip_matrix(m, "vertical"))
})

test_that("flip_matrix catches bad input", {
  expect_error(flip_matrix("foo", "horizontal"))
  expect_error(flip_matrix(as.data.frame(m)),
               "\"matrix\" is not TRUE")
  expect_error(flip_matrix(m, "foo"))
})

test_that("rotate_matrix catches bad input", {
  expect_error(rotate_matrix(as.data.frame(m), "left", 1))
  expect_error(rotate_matrix("foo", "left", 1))
  expect_error(rotate_matrix(m, "foo", 1))
  expect_error(rotate_matrix(m, "left", 0))
})
