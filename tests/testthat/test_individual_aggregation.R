# Unit tests using testthat
## testthat::test_file("tests/testthat/test_individual_aggregation.R")

describe("expand_to_individual", {
  # Create a sample dataset with S0-S3
  standard_data <- data.frame(
    tmt = c("C", "SC", "T1"),
    tank = c("5A", "4A", "3A"),
    S0 = c(2, 1, 4),
    S1 = c(1, 3, 0),
    S2 = c(0, 0, 0),
    S3 = c(1, 0, 0),
    total = c(4, 4, 4)
  )

  # Create a dataset with S0-S5
  extended_data <- data.frame(
    tmt = c("C", "SC"),
    tank = c("5A", "4A"),
    S0 = c(2, 1),
    S1 = c(1, 0),
    S2 = c(0, 1),
    S3 = c(1, 0),
    S4 = c(0, 1),
    S5 = c(0, 1),
    total = c(4, 4)
  )

  # Create a dataset with custom column names
  custom_data <- data.frame(
    treatment = c("Control", "Treatment"),
    replicate = c("R1", "R2"),
    I0 = c(2, 1),
    I1 = c(1, 2),
    I2 = c(1, 1),
    count = c(4, 4)
  )

  it("correctly expands standard data (S0-S3)", {
    result <- expand_to_individual_simple(standard_data)
    result_tidy <-   expand_to_individual_tidy(standard_data)
    expect_identical(result$data,as.data.frame(result_tidy))
    result <- result$data
    # Check dimensions
    expect_equal(nrow(result), sum(standard_data$total))
    expect_equal(ncol(result), 3)

    # Check column names
    expect_equal(names(result), c("tmt", "tank", "score"))

    # Check counts match the original data
    c_tank <- result[result$tank == "5A", ]
    expect_equal(sum(c_tank$score == "S0"), 2)
    expect_equal(sum(c_tank$score == "S1"), 1)
    expect_equal(sum(c_tank$score == "S3"), 1)
  })

  it("correctly expands extended data (S0-S5)", {
    result <- expand_to_individual_simple(extended_data)
    result_tidy <- expand_to_individual_tidy(extended_data)
    expect_identical(result$data,as.data.frame(result_tidy))
    result <- result$data
    # Check dimensions
    expect_equal(nrow(result), sum(extended_data$total))

    # Check all score categories are represented
    sc_tank <- result[result$tank == "4A", ]
    expect_equal(sum(sc_tank$score == "S0"), 1)
    expect_equal(sum(sc_tank$score == "S2"), 1)
    expect_equal(sum(sc_tank$score == "S4"), 1)
    expect_equal(sum(sc_tank$score == "S5"), 1)
  })

  it("works with custom column names", {
    result <- expand_to_individual_simple(
      custom_data,
      treatment_col = "treatment",
      replicate_col = "replicate",
      score_prefix = "I",
      total_col = "count"
    )
    result_tidy <- expand_to_individual_tidy(custom_data,
                                             treatment_col = "treatment",
                                             replicate_col = "replicate",
                                             score_prefix = "I",
                                             total_col = "count")
    expect_identical(result$data,as.data.frame(result_tidy))
    result <- result$data
    # Check dimensions
    expect_equal(nrow(result), sum(custom_data$count))

    # Check column names
    expect_equal(names(result), c("treatment", "replicate", "score"))

    # Check counts match the original data
    control_rep <- result[result$treatment == "Control" & result$replicate == "R1", ]
    expect_equal(sum(control_rep$score == "I0"), 2)
    expect_equal(sum(control_rep$score == "I1"), 1)
    expect_equal(sum(control_rep$score == "I2"), 1)
  })

  it("raises an error when required columns are missing", {
    bad_data <- standard_data[, -which(names(standard_data) == "tmt")]
    expect_error(expand_to_individual_simple(bad_data), "Treatment column 'tmt' not found")
  })

  it("raises an error when no score columns are found", {
    no_score_data <- data.frame(
      tmt = c("C", "SC"),
      tank = c("5A", "4A"),
      X0 = c(2, 1),  # Not using S prefix
      X1 = c(2, 3),
      total = c(4, 4)
    )
    expect_error(expand_to_individual_simple(no_score_data), "No score columns found with prefix")
  })

  it("gives a warning when totals don't match sum of scores", {
    inconsistent_data <- standard_data
    inconsistent_data$total[1] <- 5  # Change total but not individual counts
    expect_warning(expand_to_individual_simple(inconsistent_data),
                   "score counts that don't sum to the total")
  })
})

describe("aggregate_from_individual", {
  # Create a sample individual dataset
  standard_individual <- data.frame(
    tmt = c("C", "C", "C", "C", "SC", "SC", "SC", "SC"),
    tank = c("5A", "5A", "5A", "5A", "4A", "4A", "4A", "4A"),
    score = c("S0", "S0", "S1", "S3", "S0", "S1", "S1", "S1"),
    stringsAsFactors = FALSE
  )

  # Create a dataset with extended scores
  extended_individual <- data.frame(
    tmt = c("C", "C", "C", "C", "SC", "SC", "SC", "SC"),
    tank = c("5A", "5A", "5A", "5A", "4A", "4A", "4A", "4A"),
    score = c("S0", "S0", "S1", "S3", "S0", "S4", "S4", "S5"),
    stringsAsFactors = FALSE
  )

  # Create a dataset with custom column names
  custom_individual <- data.frame(
    treatment = c("Control", "Control", "Control", "Treatment", "Treatment"),
    replicate = c("R1", "R1", "R1", "R2", "R2"),
    injury = c("I0", "I0", "I1", "I0", "I2"),
    stringsAsFactors = FALSE
  )

  it("correctly aggregates standard individual records", {
    result <- aggregate_from_individual_simple(standard_individual)
    result_tidy <- aggregate_from_individual_tidy(standard_individual)
    expect_equal(result,as.data.frame(result_tidy)) ## not identical since
    ## `actual$S0, S1, S3` is a double vector
    ## `expected$S0, S1, S3` is an integer vector
    # Check dimensions
    expect_equal(nrow(result), 2)  # Two unique tank/treatment combinations
    expect_equal(ncol(result), 6)  # tmt, tank, S0, S1, S3, total

    # Check column names
    expect_true(all(c("tmt", "tank", "S0", "S1", "S3", "total") %in% names(result)))

    # Check counts for first row (C, 5A)
    expect_equal(result$S0[1], 2)
    expect_equal(result$S1[1], 1)
    expect_equal(result$S3[1], 1)
    expect_equal(result$total[1], 4)

    # Check counts for second row (SC, 4A)
    expect_equal(result$S0[2], 1)
    expect_equal(result$S1[2], 3)
    expect_equal(result$total[2], 4)
  })

  it("correctly aggregates extended individual records", {
    result <- aggregate_from_individual_simple(extended_individual)
    result_tidy <- aggregate_from_individual_tidy(extended_individual)
    expect_equal(result,as.data.frame(result_tidy))
    # Check all score categories are included
    expect_true(all(c("S0", "S1", "S3", "S4", "S5") %in% names(result)))

    # Check counts
    expect_equal(result$S4[2], 2)
    expect_equal(result$S5[2], 1)
  })

  it("works with custom column names", {
    result <- aggregate_from_individual_simple(
      custom_individual,
      treatment_col = "treatment",
      replicate_col = "replicate",
      score_col = "injury",
      total_col = "count"
    )
    result_tidy <- aggregate_from_individual_tidy(     custom_individual,
                                                       treatment_col = "treatment",
                                                       replicate_col = "replicate",
                                                       score_col = "injury",
                                                       total_col = "count")
    expect_equal(result,as.data.frame(result_tidy))
    # Check dimensions and column names
    expect_equal(nrow(result), 2)
    expect_true(all(c("treatment", "replicate", "I0", "I1", "I2", "count") %in% names(result)))

    # Check counts
    control_row <- result[result$treatment == "Control", ]
    expect_equal(control_row$I0, 2)
    expect_equal(control_row$I1, 1)
    expect_equal(control_row$count, 3)

    treatment_row <- result[result$treatment == "Treatment", ]
    expect_equal(treatment_row$I0, 1)
    expect_equal(treatment_row$I2, 1)
    expect_equal(treatment_row$count, 2)
  })

  it("raises an error when required columns are missing", {
    bad_data <- standard_individual[, -which(names(standard_individual) == "score")]
    expect_error(aggregate_from_individual_simple(bad_data), "Missing required columns")
  })
})



describe("convert_fish_data",{
  # Test data
  test_data <- data.frame(
    tmt = c("C", "SC"),
    tank = c("5A", "4A"),
    S0 = c(2, 1),
    S1 = c(1, 3),
    S2 = c(0, 0),  # Note: S2 has zero occurrences
    S3 = c(1, 0),
    total = c(4, 4)
  )

  # Convert to individual records
  individual_data <- convert_fish_data(test_data, direction = "to_individual")


  # Convert back to aggregated format
  aggregated_data <- convert_fish_data(individual_data, direction = "to_aggregated")


  it("converted individual data",{
    expect_identical(individual_data,structure(list(tmt = c("C", "C", "C", "C", "SC", "SC", "SC",
                                                        "SC"), tank = c("5A", "5A", "5A", "5A", "4A", "4A", "4A", "4A"
                                                        ), score = c("S0", "S0", "S1", "S3", "S0", "S1", "S1", "S1")), row.names = c(NA,
                                                                                                                                     -8L), class = "data.frame", score_columns = c("S0", "S1", "S2",
                                                                                                                                                                                   "S3")))
  })
  it("convert back to aggregated data",{
    expect_identical(aggregated_data,structure(list(tmt = c("C", "SC"), tank = c("5A", "4A"), S0 = c(2,
                                                                                                     1), S1 = c(1, 3), S2 = c(0, 0), S3 = c(1, 0), total = c(4, 4)), row.names = c(NA,
                                                                                                                                                                                   -2L), class = "data.frame"))

  })
  it("Verify that the conversion is lossless",{
    #
    all.equal(test_data[order(test_data$tmt, test_data$tank), c("tmt", "tank", "S0", "S1", "S2", "S3", "total")],
              aggregated_data[order(aggregated_data$tmt, aggregated_data$tank), c("tmt", "tank", "S0", "S1", "S2", "S3", "total")])

  })
})
