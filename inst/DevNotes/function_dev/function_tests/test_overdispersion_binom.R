describe("test_within_group_overdispersion_combined", {

  # Create test data
  test_data <- data.frame(
    Treatment = rep(c("A", "B", "C"), each = 3),
    Successes = c(10, 12, 8, 15, 14, 16, 20, 19, 21),
    Trials = c(20, 20, 20, 30, 30, 30, 40, 40, 40)
  )

  # Single replicate data
  single_rep_data <- data.frame(
    Treatment = c("A", "B", "C"),
    Successes = c(10, 15, 20),
    Trials = c(20, 30, 40)
  )

  # Overdispersed data
  overdispersed_data <- data.frame(
    Treatment = rep(c("A"), each = 5),
    Successes = c(2, 8, 10, 12, 18),  # Highly variable success rates
    Trials = c(20, 20, 20, 20, 20)
  )

  it("returns the correct structure with z-test method", {
    result <- test_within_group_overdispersion_combined(test_data, Successes, Trials, method = "z-test")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)  # One row per treatment
    expect_equal(ncol(result), 7)  # 7 columns in output

    expected_cols <- c("Treatment", "n_replicates", "statistic", "df",
                       "p_value", "overdispersed", "test_method")
    expect_named(result, expected_cols)

    expect_true(all(result$n_replicates == 3))
    expect_true(all(result$test_method == "Tarone's Z-test"))
  })

  it("returns the correct structure with chi-sq method", {
    result <- test_within_group_overdispersion_combined(test_data, Successes, Trials, method = "chi-sq")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 7)

    expect_true(all(result$n_replicates == 3))
    expect_true(all(result$test_method == "Chi-square goodness of fit"))
    expect_true(all(result$df == 2))  # df = n_replicates - 1
  })

  it("handles single replicate groups correctly", {
    result <- test_within_group_overdispersion_combined(single_rep_data, Successes, Trials)

    expect_equal(nrow(result), 3)
    expect_true(all(is.na(result$statistic)))
    expect_true(all(is.na(result$p_value)))
    expect_true(all(!result$overdispersed))
    expect_true(all(grepl("Insufficient replicates", result$test_method)))
  })

  it("detects overdispersion when present", {
    result <- test_within_group_overdispersion_combined(overdispersed_data, Successes, Trials)

    # The highly variable data should be detected as overdispersed
    expect_true(any(result$overdispersed))
    expect_lt(result$p_value[1], 0.05)
  })

  it("handles method argument correctly", {
    expect_error(test_within_group_overdispersion_combined(test_data, Successes, Trials, method = "invalid"),
                 "'arg' should be one of")

    # Default should be z-test
    default_result <- test_within_group_overdispersion_combined(test_data, Successes, Trials)
    expect_true(all(default_result$test_method == "Tarone's Z-test"))
  })
})
