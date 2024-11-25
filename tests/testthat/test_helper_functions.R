# test_logxp.R
context("Testing logxp and invlogxp functions")

library(testthat)

test_that("logxp function scales the x-axis correctly", {
  x <- c(1, 2, 3)
  a <- 1
  result <- logxp(x, a)
  expect_equal(result, log(a + x))
})

test_that("invlogxp function inversely transforms the scaled x-axis", {
  x <- c(1, 2, 3)
  a <- 1
  result <- invlogxp(log(x + a), a)
  expect_equal(result, x)
})

test_that("simplifyTreatment",{
  x <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
                   3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L,
                   1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L,
                   5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,
                   2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L,
                   6L), levels = c("Control", "1", "3.51", "12.300000000000001",
                                   "42.9", "150"), class = "factor")
  x1 <- simplifyTreatment(x)
  expect_identical(levels(x1),c("Control", "1", "3.51", "12.3", "42.9", "150"))
})
