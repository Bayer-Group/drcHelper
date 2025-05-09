# tests/testthat/test-broom_functions.R

describe("broom_williams", {
  # Create a simple dataset for testing
  set.seed(123)
  test_data <- data.frame(
    dose = factor(rep(c(0, 1, 5, 10), each = 5)),
    response = c(
      rnorm(5, 100, 10),  # Control
      rnorm(5, 90, 10),   # Dose 1
      rnorm(5, 80, 10),   # Dose 5
      rnorm(5, 70, 10)    # Dose 10
    )
  )

  it("returns a tibble with the expected columns for PMCMRplus method", {
    result <- broom_williams(
      response ~ dose,
      data = test_data,
      method = "Williams_PMCMRplus",
      alternative = "less"
    )

    expect_s3_class(result, "tbl_df")
    expected_columns <- c("comparison", "estimate", "p.value", "conf.low", "conf.high", "method")
    expect_true(all(expected_columns %in% colnames(result)))
    expect_equal(result$method[1], "Williams_PMCMRplus")
  })

  it("handles errors gracefully", {
    # Testing with bad data (only one observation per group)
    bad_data <- data.frame(
      dose = factor(c(0, 1, 5, 10)),
      response = c(100, 90, 80, 70)
    )

    expect_warning({
      result <- broom_williams(
        response ~ dose,
        data = bad_data,
        method = "Williams_PMCMRplus"
      )
    })

    # Should still return a tibble with the correct structure
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

describe("broom_dunnett", {
  # Create a simple dataset for testing
  set.seed(123)
  test_data <- data.frame(
    dose = factor(rep(c(0, 1, 5, 10), each = 5)),
    response = c(
      rnorm(5, 100, 10),  # Control
      rnorm(5, 90, 10),   # Dose 1
      rnorm(5, 80, 10),   # Dose 5
      rnorm(5, 70, 10)    # Dose 10
    )
  )

  it("returns a tibble with the expected columns for DescTools method", {
    result <- broom_dunnett(
      response ~ dose,
      data = test_data,
      method = "Dunnett_DescTools",
      control = "0"
    )

    expect_s3_class(result, "tbl_df")
    expected_columns <- c("comparison", "estimate", "p.value", "conf.low", "conf.high", "method")
    expect_true(all(expected_columns %in% colnames(result)))
    expect_equal(result$method[1], "Dunnett_DescTools")
  })

  it("returns a tibble with the expected columns for PMCMRplus method", {
    result <- broom_dunnett(
      response ~ dose,
      data = test_data,
      method = "Dunnett_PMCMRplus",
      control = "0"
    )

    expect_s3_class(result, "tbl_df")
    expected_columns <- c("comparison", "estimate", "p.value", "conf.low", "conf.high", "method")
    expect_true(all(expected_columns %in% colnames(result)))
    expect_equal(result$method[1], "Dunnett_PMCMRplus")
  })

  it("handles errors gracefully", {
    # Testing with bad data (only one observation per group)
    bad_data <- data.frame(
      dose = factor(c(0, 1, 5, 10)),
      response = c(100, 90, 80, 70)
    )

    expect_warning({
      result <- broom_dunnett(
        response ~ dose,
        data = bad_data,
        method = "Dunnett_DescTools",
        control = "0"
      )
    })

    # Should still return a tibble with the correct structure
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

describe("broom_stepDown", {
  # Create a simple dataset for testing
  set.seed(123)
  test_data <- data.frame(
    dose = factor(rep(c(0, 1, 5, 10), each = 5)),
    response = c(
      rnorm(5, 100, 10),  # Control
      rnorm(5, 90, 10),   # Dose 1
      rnorm(5, 80, 10),   # Dose 5
      rnorm(5, 70, 10)    # Dose 10
    )
  )

  it("returns a tibble with the expected columns", {
    result <- broom_stepDown(
      response ~ dose,
      data = test_data,
      method = "stepDown_PMCMR",
      test = "jonckheereTest"
    )

    expect_s3_class(result, "tbl_df")
    expected_columns <- c("comparison", "p.value", "method", "statistic")
    expect_true(all(expected_columns %in% colnames(result)))
    expect_match(result$method[1], "stepDown_jonckheereTest")
  })

  it("handles errors gracefully", {
    # Testing with bad data (only one observation per group)
    bad_data <- data.frame(
      dose = factor(c(0, 1, 5, 10)),
      response = c(100, 90, 80, 70)
    )

    expect_warning({
      result <- broom_stepDown(
        response ~ dose,
        data = bad_data,
        method = "stepDown_PMCMR"
      )
    })

    # Should still return a tibble with the correct structure
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

describe("combine_multiple_comparisons", {
  # Create a simple dataset for testing
  set.seed(123)
  test_data <- data.frame(
    dose = factor(rep(c(0, 1, 5, 10), each = 5)),
    response = c(
      rnorm(5, 100, 10),  # Control
      rnorm(5, 90, 10),   # Dose 1
      rnorm(5, 80, 10),   # Dose 5
      rnorm(5, 70, 10)    # Dose 10
    )
  )

  it("returns a list with detailed results and comparison table", {
    result <- combine_multiple_comparisons(
      data = test_data,
      response = response,
      dose = dose,
      control = "0",
      alternative = "less"
    )

    expect_type(result, "list")
    expect_true("detailed_results" %in% names(result))
    expect_true("comparison_table" %in% names(result))

    expect_s3_class(result$detailed_results, "tbl_df")
    expect_s3_class(result$comparison_table, "tbl_df")
  })

  it("includes results from all specified methods", {
    methods <- c("Dunnett_multcomp", "Williams_PMCMRplus", "stepDown_PMCMR")

    result <- combine_multiple_comparisons(
      data = test_data,
      response = response,
      dose = dose,
      control = "0",
      methods = methods
    )

    # Check that all methods are included in the detailed results
    unique_methods <- unique(result$detailed_results$method)
    for (method in methods) {
      expect_true(any(grepl(method, unique_methods)))
    }

    # Check that the comparison table has columns for all methods
    for (method in methods) {
      expect_true(any(grepl(paste0("p.value_", method), colnames(result$comparison_table))))
      expect_true(any(grepl(paste0("significant_", method), colnames(result$comparison_table))))
    }
  })

  it("handles grouped data correctly", {
    # Create grouped test data
    grouped_test_data <- data.frame(
      dose = factor(rep(c(0, 1, 5, 10), each = 6)),
      group = rep(rep(c("A", "B"), each = 3), 4),
      response = c(
        rnorm(6, 100, 10),  # Control
        rnorm(6, 90, 10),   # Dose 1
        rnorm(6, 80, 10),   # Dose 5
        rnorm(6, 70, 10)    # Dose 10
      )
    )

    # Should run without errors with grouping
    expect_error({
      result <- combine_multiple_comparisons(
        data = grouped_test_data,
        response = response,
        dose = dose,
        group = group,
        control = "0",
        alternative = "less"
      )
    }, NA)

    # Check that the results include the group information
    expect_s3_class(result$detailed_results, "tbl_df")
    expect_s3_class(result$comparison_table, "tbl_df")
  })
})



#### Quantal Data


# Example: Calculating NOEC for a toxicity test dataset
library(dplyr)
library(ggplot2)
library(rstatix)

# Create a simulated toxicity test dataset
set.seed(456)
toxicity_data <- data.frame(
  concentration = rep(c(0, 0.1, 1, 5, 10, 50, 100), each = 4),
  survival = c(
    rbinom(4, 20, 0.95),  # Control: ~95% survival
    rbinom(4, 20, 0.93),  # 0.1 mg/L: ~93% survival
    rbinom(4, 20, 0.90),  # 1 mg/L: ~90% survival
    rbinom(4, 20, 0.85),  # 5 mg/L: ~85% survival
    rbinom(4, 20, 0.70),  # 10 mg/L: ~70% survival
    rbinom(4, 20, 0.40),  # 50 mg/L: ~40% survival
    rbinom(4, 20, 0.15)   # 100 mg/L: ~15% survival
  )
)

# Calculate survival percentage
toxicity_data <- toxicity_data %>%
  mutate(
    total = 20,  # Assuming 20 organisms per replicate
    survival_pct = (survival / total) * 100,
    concentration = factor(concentration)
  )
res1 <- toxicity_data %>% t_test(survival_pct ~ concentration, ref.group = "0")
res1.0 <- toxicity_data %>% pairwise_t_test(survival_pct ~ concentration, ref.group = "0")
## strangely, res1.0 and res1 give different results
## in terms of both unadjusted p and adjusted p-values.
## in this case p.adjust is not working
res2 <- PMCMRplus::welchManyOneTTest(survival_pct ~ concentration, data=toxicity_data)
testthat::expect_equal(as.numeric(res2$p.value),as.numeric(res1$p.adj),tolerance=0.005)
## check the t test results
t.test(survival_pct ~ concentration,data=toxicity_data%>%
         filter(concentration=="0" | concentration =="5"))

res1 <- toxicity_data %>% wilcox_test(survival_pct ~ concentration, ref.group = "0")
res1.0 <- toxicity_data %>% pairwise_wilcox_test(survival_pct ~ concentration, ref.group = "0")
res1.0
# Calculate NOEC using t-test
noec_t <- calculate_noec_rstatix(
  data = toxicity_data,
  response = survival_pct,
  dose = concentration,
  control = "0",
  test = "t.test",
  alternative = "greater"  # Survival decreases with increasing concentration
)

# Calculate NOEC using Wilcoxon test
noec_w <- calculate_noec_rstatix(
  data = toxicity_data,
  response = survival_pct,
  dose = concentration,
  control = "0",
  test = "wilcox.test",
  alternative = "greater"
)

# Compare NOEC values from different methods
noec_comparison <- compare_noec_methods(
  data = toxicity_data,
  response = survival_pct,
  dose = concentration,
  control = "0"
)

# Print results
cat("NOEC from t-test:", noec_t$noec, "mg/L\n")
cat("NOEC from Wilcoxon test:", noec_w$noec, "mg/L\n")

# View detailed comparison
print(noec_comparison$noec_values)

# Plot the results
plot1 <- plot_noec(
  data = toxicity_data,
  response = survival_pct,
  dose = concentration,
  noec = noec_t$noec,
  title = "Survival vs. Concentration (NOEC from t-test)"
)

print(plot1)
print(noec_comparison$plot)
