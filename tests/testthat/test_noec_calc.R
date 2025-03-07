# tests/testthat/test-calculate_noec_rstatix.R

describe("calculate_noec_rstatix", {



  # Create a dataset with clear dose-response relationship
  create_test_data <- function(effect_threshold = 3, n_per_group = 5) {
    doses <- c("0", "1", "2", "3", "4", "5")
    data <- data.frame(
      dose = rep(doses, each = n_per_group),
      response = numeric(length(doses) * n_per_group)
    )

    # Control group (dose 0)
    data$response[data$dose == "0"] <- rnorm(n_per_group, mean = 10, sd = 1)

    # Low doses (no effect)
    data$response[data$dose == "1"] <- rnorm(n_per_group, mean = 10, sd = 1)
    data$response[data$dose == "2"] <- rnorm(n_per_group, mean = 10, sd = 1)

    # Doses at and above threshold (with effect)
    data$response[data$dose == "3"] <- rnorm(n_per_group, mean = 8, sd = 1)
    data$response[data$dose == "4"] <- rnorm(n_per_group, mean = 6, sd = 1)
    data$response[data$dose == "5"] <- rnorm(n_per_group, mean = 4, sd = 1)

    return(data)
  }

  # Create a dataset with no significant effects
  create_no_effect_data <- function(n_per_group = 5) {
    doses <- c("0", "1", "2", "3", "4", "5")
    data <- data.frame(
      dose = rep(doses, each = n_per_group),
      response = numeric(length(doses) * n_per_group)
    )

    # All groups have similar response
    for (d in doses) {
      data$response[data$dose == d] <- rnorm(n_per_group, mean = 10, sd = 1)
    }

    return(data)
  }

  # Create a dataset where all doses have an effect
  create_all_effect_data <- function(n_per_group = 5) {
    doses <- c("0", "1", "2", "3", "4", "5")
    data <- data.frame(
      dose = rep(doses, each = n_per_group),
      response = numeric(length(doses) * n_per_group)
    )

    # Control group
    data$response[data$dose == "0"] <- rnorm(n_per_group, mean = 10, sd = 0.5)

    # All other doses have an effect
    data$response[data$dose == "1"] <- rnorm(n_per_group, mean = 8, sd = 0.5)
    data$response[data$dose == "2"] <- rnorm(n_per_group, mean = 7, sd = 0.5)
    data$response[data$dose == "3"] <- rnorm(n_per_group, mean = 6, sd = 0.5)
    data$response[data$dose == "4"] <- rnorm(n_per_group, mean = 5, sd = 0.5)
    data$response[data$dose == "5"] <- rnorm(n_per_group, mean = 4, sd = 0.5)

    return(data)
  }

  # Generate the test datasets
  # Generate test data
  set.seed(123)
  test_data <- create_test_data()
  no_effect_data <- create_no_effect_data()
  all_effect_data <- create_all_effect_data()

  it("correctly identifies NOEC with t-test", {
    result <- calculate_noec_rstatix(test_data, response, dose, control = "0", test = "t.test")
    expect_type(result, "list")
    expect_true(result$noec %in%  c(2,3))
  })

  it("correctly identifies NOEC with wilcox test", {
    result <- calculate_noec_rstatix(test_data, response, dose, control = "0", test = "wilcox.test")
    expect_type(result, "list")
    # The exact NOEC might differ between t-test and wilcox test due to different sensitivities
    expect_true(result$noec %in% c(1, 2, 3))
  })

  it("handles data with no significant effects", {
    result <- calculate_noec_rstatix(no_effect_data, response, dose, control = "0", test = "t.test")
    expect_equal(result$noec, 5)
    expect_equal(result$noec_message, "No significant effects detected at any dose level")
  })

  it("handles data where all doses have effects", {
    result <- calculate_noec_rstatix(all_effect_data, response, dose, control = "0", test = "t.test")
    expect_equal(result$noec, 0)
    expect_equal(result$noec_message, "All tested doses showed significant effects")
  })

  it("works with different alpha levels", {
    # With a very strict alpha, fewer effects might be detected
    strict_result <- calculate_noec_rstatix(test_data, response, dose,
                                            control = "0", test = "t.test",
                                            alpha = 0.01)

    # With a very lenient alpha, more effects might be detected
    lenient_result <- calculate_noec_rstatix(test_data, response, dose,
                                             control = "0", test = "t.test",
                                             alpha = 0.1)

    # The strict test should have a higher or equal NOEC than the lenient test
    expect_true(strict_result$noec >= lenient_result$noec)
  })

  it("handles different p-value adjustment methods", {
    result1 <- calculate_noec_rstatix(test_data, response, dose,
                                      control = "0", test = "t.test",
                                      p_adjust_method = "bonferroni")

    result2 <- calculate_noec_rstatix(test_data, response, dose,
                                      control = "0", test = "t.test",
                                      p_adjust_method = "none")

    # Results might differ based on adjustment method
    expect_type(result1$noec, "double")
    expect_type(result2$noec, "double")
    expect_identical(result2$test_results$p.adj,result1$test_results$p)
  })

  it("handles character dose levels correctly", {
    # Create data with character doses
    char_data <- test_data
    char_data$dose <- as.character(char_data$dose)

    result <- calculate_noec_rstatix(char_data, response, dose, control = "0", test = "t.test")
    expect_type(result, "list")
    expect_true(result$noec %in%  c(2,3))
  })

  it("handles factor dose levels correctly", {
    # Create data with factor doses
    factor_data <- test_data
    factor_data$dose <- as.factor(factor_data$dose)

    result <- calculate_noec_rstatix(factor_data, response, dose, control = "0", test = "t.test")
    expect_type(result, "list")
    expect_true(result$noec %in%  c(2,3))
  })
})
