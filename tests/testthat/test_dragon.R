context("dragon curve")


test_that("dragon is folded as expected", {
  expect_identical(fold_dragon(1), c(1))
  expect_identical(fold_dragon(2), c(1, 1, -1))
  expect_identical(fold_dragon(3), c(1, 1, -1, 1, 1, -1, -1))
})

