#' @importFrom testthat describe it expect_equal expect_error expect_warning expect_true expect_false
NULL

describe("cochranArmitageTrendTest", {
  it("calculates correct test statistic and p-value", {
    # Example from literature with known result
    successes <- c(10, 12, 15, 18)
    totals <- c(20, 20, 20, 20)
    doses <- c(0, 1, 2, 3)

    result <- cochranArmitageTrendTest(successes, totals, doses)

    # The test statistic should be positive for increasing trend
    expect_true(result$statistic > 0)
    # P-value should be significant for this example
    expect_lt(result$p.value, 0.05)
  })

  it("applies Rao-Scott correction correctly", {
    successes <- c(10, 12, 15, 18)
    totals <- c(20, 20, 20, 20)
    doses <- c(0, 1, 2, 3)

    # Without correction
    result1 <- cochranArmitageTrendTest(successes, totals, doses)

    # With Rao-Scott correction
    result2 <- cochranArmitageTrendTest(successes, totals, doses, rao_scott = TRUE)

    # Check that phi is calculated and > 1
    expect_true("phi" %in% names(result2))
    expect_true(result2$phi >= 1)

    # The test statistic should be smaller with correction
    expect_lt(abs(result2$statistic), abs(result1$statistic))
  })

  it("handles different alternative hypotheses", {
    successes <- c(10, 12, 15, 18)  # Increasing trend
    totals <- c(20, 20, 20, 20)
    doses <- c(0, 1, 2, 3)

    # Two-sided
    result1 <- cochranArmitageTrendTest(successes, totals, doses, alternative = "two.sided")

    # Greater (increasing trend)
    result2 <- cochranArmitageTrendTest(successes, totals, doses, alternative = "greater")

    # Less (decreasing trend)
    result3 <- cochranArmitageTrendTest(successes, totals, doses, alternative = "less")

    # For increasing trend, "greater" should have smaller p-value than "two.sided"
    expect_lt(result2$p.value, result1$p.value)

    # "less" should have larger p-value for increasing trend
    expect_gt(result3$p.value, result1$p.value)
  })

  it("handles input validation correctly", {
    # Test for unequal vector lengths
    expect_error(
      cochranArmitageTrendTest(c(10, 8, 6), c(10, 10), c(0, 1, 2)),
      "'successes', 'totals', and 'doses' must have the same length"
    )

    # Test for successes > totals
    expect_error(
      cochranArmitageTrendTest(c(11, 8, 6), c(10, 10, 10), c(0, 1, 2)),
      "'successes' cannot be greater than 'totals'"
    )

    # Test for insufficient groups
    expect_error(
      cochranArmitageTrendTest(c(10), c(10), c(0)),
      "At least two groups required"
    )
  })
})

describe("stepDownTrendTestBinom", {
  it("correctly identifies NOEC and LOEC with decreasing trend", {
    # Simulated data with clear dose-response
    successes <- c(20, 18, 15, 10, 5)
    totals <- rep(20, 5)
    doses <- c(0, 1, 2, 5, 10)

    result <- stepDownTrendTestBinom(successes, totals, doses)

    # We expect a significant trend overall
    expect_lt(result$p.value[1, 1], 0.05)

    # Check that NOEC and LOEC are identified (exact values depend on data)
    expect_true(!is.na(result$noec) || !is.na(result$loec))
  })

  it("handles the case with no significant trend", {
    # Simulated data with no trend
    successes <- c(18, 19, 18, 19, 18)
    totals <- rep(20, 5)
    doses <- c(0, 1, 2, 5, 10)

    result <- stepDownTrendTestBinom(successes, totals, doses)

    # We expect no significant trend
    expect_gt(result$p.value[1, 1], 0.05)

    # NOEC should be the highest dose, LOEC should be NA
    expect_equal(result$noec, 10)
    expect_true(is.na(result$loec))
  })

  it("applies Rao-Scott correction correctly", {
    # Simulated data with overdispersion
    successes <- c(20, 18, 15, 10, 5)
    totals <- rep(20, 5)
    doses <- c(0, 1, 2, 5, 10)

    # Without correction
    result1 <- stepDownTrendTestBinom(successes, totals, doses)

    # With Rao-Scott correction
    result2 <- stepDownTrendTestBinom(successes, totals, doses, rao_scott = TRUE)

    # The p-value with correction should be larger (more conservative)
    expect_true(result2$p.value[1, 1] >= result1$p.value[1, 1])
  })

  it("handles input validation correctly", {
    # Test for unequal vector lengths
    expect_error(
      stepDownTrendTestBinom(c(10, 8, 6), c(10, 10), c(0, 1, 2)),
      "'successes', 'totals', and 'doses' must have the same length"
    )

    # Test for successes > totals
    expect_error(
      stepDownTrendTestBinom(c(11, 8, 6), c(10, 10, 10), c(0, 1, 2)),
      "'successes' cannot be greater than 'totals'"
    )

    # Test for insufficient groups
    expect_error(
      stepDownTrendTestBinom(c(10), c(10), c(0)),
      "at least two dose groups are required"
    )
  })
})
