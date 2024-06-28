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
