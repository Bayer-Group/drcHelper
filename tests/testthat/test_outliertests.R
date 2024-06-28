# test_dixon.R
context("Testing Dixon's test for outliers")

library(outliers)

test_that("Dixon's test correctly identifies outliers", {
  y0 <- c(2.8, 2.8, 3.1, 4, 4.3, 4)
  result <- dixon.test(y0, type = 10, opposite = FALSE, two.sided = TRUE)

  # Add your assertions here based on the expected behavior of the dixon.test function
  expect_true(result$p.value > 0.05,
              info = "p-value is greater than 0.05, indicating no outliers")
  # Add more assertions as needed
})
