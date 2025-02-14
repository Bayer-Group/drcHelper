## test internal functions


describe("calpha.test",{
  it("calpha.test calculates correctly for incidence data", {
    # Mock data for testing
    my_fisher <- list(N = 10, n = 5, index = 0.8, flavor = "incidence")
    class(my_fisher) <- "fisher"
    result <- drcHelper:::calpha.test(my_fisher)

    expect_s3_class(result, "htest")
    expect_named(result, c("statistic", "p.value", "method", "data.name"))
    expect_equal(result$method, "C(alpha) test")

    # Check if the statistic is calculated correctly
    expected_statistic <- (5 * (10 - 1) * 0.8 - 10 * 5) / sqrt(2 * 10 * 5 * (5 - 1))
    expect_equal(as.numeric(result$statistic["z"]), expected_statistic, tolerance = 1e-6)

    # Check if p-value is calculated correctly
    expected_pval <- 2 * pnorm(abs(expected_statistic), lower.tail = FALSE)
    expect_equal(result$p.value, expected_pval, tolerance = 1e-6)
  })
  it("calpha.test stops for invalid input", {
    invalid_input <- list(N = 10, flavor = "incidence")
    class(invalid_input) <- "fisher"
    expect_error(drcHelper:::calpha.test(invalid_input), "Input object must contain 'N', 'n', and 'index'.")
  }
  )
})
