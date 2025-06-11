## using BDD testing

describe("create_contingency_table", {
  library(dplyr)
  it("correctly creates a contingency table with default settings", {
    # Create test data
    test_data <- data.frame(
      dose = c(0, 0, 1, 1),
      alive = c(8, 9, 5, 6),
      dead = c(2, 1, 5, 4)
    )

    # Expected result with default settings
    expected <- matrix(c(17, 11, 3, 9), nrow = 2,
                       dimnames = list(c("dose_0", "dose_1"), c("success", "failure")))

    # Test function
    result <- create_contingency_table(test_data, "dose", "alive", "dead")

    expect_equal(result, expected)
  })

  it("correctly creates a contingency table with custom settings", {
    # Create test data
    test_data <- data.frame(
      treatment = c("A", "A", "B", "B"),
      survived = c(8, 9, 5, 6),
      died = c(2, 1, 5, 4)
    )

    # Expected result with custom settings
    expected <- matrix(c(17, 11, 3, 9), nrow = 2,
                       dimnames = list(c("group_A", "group_B"), c("survived", "died")))

    # Test function
    result <- create_contingency_table(
      test_data,
      "treatment",
      "survived",
      "died",
      prefix = "group_",
      col_names = c("survived", "died")
    )

    expect_equal(result, expected)
  })

  it("throws an error when specified columns are not in the data", {
    test_data <- data.frame(
      dose = c(0, 0, 1, 1),
      survived = c(8, 9, 5, 6),  # Different column name
      dead = c(2, 1, 5, 4)
    )

    expect_error(create_contingency_table(test_data, "dose", "alive", "dead"),
                 "One or more specified columns not found in the data")
  })

  it("throws an error when col_names is not of length 2", {
    test_data <- data.frame(
      dose = c(0, 0, 1, 1),
      alive = c(8, 9, 5, 6),
      dead = c(2, 1, 5, 4)
    )

    expect_error(create_contingency_table(test_data, "dose", "alive", "dead",
                                          col_names = c("success")),
                 "col_names must be a character vector of length 2")
  })
})




describe("compare_to_control_fisher", {
  library(testthat)
  it("correctly performs Fisher's exact test with specified column names", {
    # Create test data
    set.seed(123)
    test_data <- data.frame(
      treatment = c(rep("control", 3), rep("low", 3), rep("high", 3)),
      survived = c(9, 8, 10, 7, 6, 5, 3, 4, 2),
      died = c(1, 2, 0, 3, 4, 5, 7, 6, 8)
    )

    # Run the function
    result <- compare_to_control_fisher(test_data, "treatment", "survived", "died",
                                        control_level = "control")

    # Check structure
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)  # Two non-control treatments
    expect_equal(result$treatment, c("low", "high"))
    expect_true(all(c("p_value", "odds_ratio", "ci_lower", "ci_upper", "p_adjusted") %in% names(result)))

    # Verify results manually for one treatment
    level <- "low"
    control_success <- sum(test_data$survived[test_data$treatment == "control"])
    control_failure <- sum(test_data$died[test_data$treatment == "control"])
    test_success <- sum(test_data$survived[test_data$treatment == level])
    test_failure <- sum(test_data$died[test_data$treatment == level])
    cont_table <- matrix(c(control_success, control_failure, test_success, test_failure), nrow = 2, byrow = TRUE)
    manual_test <- fisher.test(cont_table)

    expect_equal(round(result$p_value[result$treatment == level], 4),
                 round(manual_test$p.value, 4))
  })

  it("uses the first level as default control if not specified", {
    test_data <- data.frame(
      dose = c(0, 0, 1, 1, 2, 2),
      alive = c(8, 9, 5, 6, 3, 4),
      dead = c(2, 1, 5, 4, 7, 6)
    )

    result <- compare_to_control_fisher(test_data, "dose", "alive", "dead")
    expect_equal(nrow(result), 2)  # Two non-control doses
    expect_equal(result$dose, c(1, 2))
  })

  it("throws an error when control level is not in the data", {
    test_data <- data.frame(
      dose = c(1, 1, 2, 2),
      alive = c(8, 9, 5, 6),
      dead = c(2, 1, 5, 4)
    )

    expect_error(compare_to_control_fisher(test_data, "dose", "alive", "dead", control_level = 0),
                 "Control level 0 not found in the data")
  })

  it("throws an error when specified columns are not in the data", {
    test_data <- data.frame(
      dose = c(0, 0, 1, 1),
      survived = c(8, 9, 5, 6),
      dead = c(2, 1, 5, 4)
    )

    expect_error(compare_to_control_fisher(test_data, "dose", "alive", "dead"),
                 "One or more specified columns not found in the data")
  })
})




describe("calcTaronesTest",{
  it("Tarone's Test can be calculated",{
    mymatrix1 <- matrix(c(4,5,5,103),nrow=2,byrow=TRUE)
    colnames(mymatrix1) <- c("Disease","Control")
    rownames(mymatrix1) <- c("Exposure","Unexposed")
    mymatrix2 <- matrix(c(10,3,5,43),nrow=2,byrow=TRUE)
    colnames(mymatrix2) <- c("Disease","Control")
    rownames(mymatrix2) <- c("Exposure","Unexposed")
    mylist <- list(mymatrix1,mymatrix2)
    res <- calcTaronesTest(mylist)
    expect_equal(res$pval,0.6274207,tolerance=1e-4)
    ## calcTaronesTest(mymatrix1)
  })
  it("Tarone's Test can not be run if there is less than 2 stratas",{

    expect_error( calcTaronesTest(mymatrix1))
  })
})



describe("Tarone.test",{
  it("Tarone.test calculates correctly for valid inputs", {
    N <- c(30, 32, 40, 28, 29, 35, 30, 34, 31, 39)
    M <- c(9, 10, 22, 15, 8, 19, 16, 19, 15, 10)

    result <- Tarone.test(N, M)

    expect_s3_class(result, "htest")
    expect_named(result, c("method", "data.name", "null.value", "alternative", "estimate", "statistic", "p.value"))
    expect_equal(result$method, "Tarone's Z test")

    # Check if the statistic is calculated correctly (you may want to calculate expected values)
    expected_statistic <- ((sum((M - N * (sum(M) / sum(N)))^2 / ((sum(M) / sum(N)) * (1 - (sum(M) / sum(N)))))) - sum(N)) / sqrt(2 * sum(N * (N - 1)))
    expect_equal(as.numeric(result$statistic), expected_statistic, tolerance = 1e-6)
  })

  it("Tarone.test stops for invalid inputs", {
    expect_error(Tarone.test(c(30, 32, 40), c(9, 10, -1)), "Count values cannot be negative")
    expect_error(Tarone.test(c(30, 32, 40), c(9, 10, 50)), "Observed count value exceeds number of trials")
    expect_error(Tarone.test(c(30.5, 32, 40), c(9, 10, 22)), "Number of trials should be integers")
    expect_error(Tarone.test(c(30, 32, 40), c(9, 10, 22.5)), "Count values should be integers")
  })
})


describe()
