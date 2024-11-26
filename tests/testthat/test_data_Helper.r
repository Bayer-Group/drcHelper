# FILE: tests/testthat/test_data_Helper.R

library(testthat)
library(drcHelper)

test_that("simplifyTreatment works with factor input", {
  x <- factor(c("Control", "1", "3.51", "12.300000000000001", "42.9", "150"))
  result <- simplifyTreatment(x)
  expected <- factor(c("Control", 1, 3.51, 12.3, 42.9, 150))
  expect_equal(result, expected)
})

test_that("simplifyTreatment works with character input", {
  x <- c("Control", "1", "3.51", "12.300000000000001", "42.9", "150")
  result <- simplifyTreatment(x)
  expected <- c("Control", 1, 3.51, 12.3, 42.9, 150)
  expect_equal(result, expected)
})

test_that("simplifyTreatment handles numeric input", {
  x <- c("1", "2", "3.51", "12.300000000000001", "42.9", "150")
  result <- simplifyTreatment(x)
  expected <- as.character(c(1, 2, 3.51, 12.3, 42.9, 150))
  expect_equal(result, expected)
})

test_that("simplifyTreatment handles mixed input", {
  x <- c("Control", "1", "3.51", "12.300000000000001", "42.9", "150")
  result <- simplifyTreatment(x)
  expected <- c("Control", 1, 3.51, 12.3, 42.9, 150)
  expect_equal(result, expected)
})


test_that("wide2long function works correctly", {
  # Create a sample wide dataset
  widedat <- data.frame(
    Replicates = c("Rep1", "Rep2", "Rep3"),
    Treatment1 = c(1.1, 2.2, 3.3),
    Treatment2 = c(4.4, 5.5, 6.6)
  )
  widedat2 <- apply(widedat,2,as.character)
  widedat2 <- rbind(colnames(widedat),widedat2)
  colnames(widedat2) <- NULL
  # Test with cnames=1, repnames=1
  longdat <- drcHelper:::wide2long(data.frame(widedat2),cnames=1,repnames = 1)
  expect_equal(ncol(longdat), 3)
  expect_equal(colnames(longdat), c("Replicates", "Treatment", "Response"))
  expect_equal(nrow(longdat), 6)

  ## longdat <- suppressWarnings(drcHelper:::wide2long(data.frame(widedat2),cnames=1,repnames = 0))
  ## expect to throw a warning when ignore the Replicates column
  expect_warning(drcHelper:::wide2long(data.frame(widedat2),cnames=1,repnames = 0),"NAs introduced by coercion") 

  # Test with cnames=0, repnames=1
  longdat <- drcHelper:::wide2long(widedat,cnames=0,repnames = 1)
  expect_equal(ncol(longdat), 3)
  expect_equal(colnames(longdat), c("Replicates", "Treatment", "Response"))
  expect_equal(nrow(longdat), 6)
  
  # Test with cnames = 0 and repnames = 0
  longdat <- wide2long(widedat, cnames = 0, repnames = 1)
  expect_equal(ncol(longdat), 3)
  expect_equal(colnames(longdat), c("Replicates","Treatment", "Response"))
  expect_equal(nrow(longdat), 6)


})