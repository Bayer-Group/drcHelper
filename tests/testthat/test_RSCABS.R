library(testthat)
## testthat::test_file("tests/testthat/test_RSCABS.R")


# Unit tests for step_down_RSCABS function

describe("step_down_RSCABS", {
  # Create a sample dataset
  test_data <- data.frame(
    tmt = c(rep("Control", 8), rep("Low", 4), rep("Medium", 4), rep("High", 4)),
    tank = c(paste0("C", 1:8), paste0("L", 1:4), paste0("M", 1:4), paste0("H", 1:4)),
    S0 = c(3, 2, 3, 2, 2, 1, 2, 3, 3, 3, 4, 2, 2, 3, 2, 2, 2, 3, 1, 2),
    S1 = c(1, 2, 0, 1, 2, 2, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 2, 0),
    S2 = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1),
    S3 = c(0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1),
    total = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
  )
  it("returns an object of class StepDownRSCABS", {
    result <- suppressWarnings({step_down_RSCABS(
      test_data,
      treatment_order = c("Control", "Low", "Medium", "High")
    )
    })

    expect_true(inherits(result, "StepDownRSCABS"))
    expect_equal(names(result), c("combined_results", "step_results", "summary",
                                  "lowest_significant", "parameters"))
  })

  it("performs the correct number of steps", {
    result <- suppressWarnings({step_down_RSCABS(
      test_data,
      treatment_order = c("Control", "Low", "Medium", "High")
    )})

    # Should have 3 steps: All, No High, Control+Low only
    expect_equal(length(result$step_results), 3)
    expect_equal(length(result$summary), 3)
    expect_equal(unique(result$combined_results$Step), 1:3)
  })

  it("correctly identifies the lowest significant treatment level", {
    result <- suppressWarnings({step_down_RSCABS(
      test_data,
      treatment_order = c("Control", "Low", "Medium", "High")
    )})


    expect_equal(result$lowest_significant$step, NA)
    expect_equal(result$lowest_significant$treatment, NA)
  })

    it("handles custom treatment order", {
      # Test with reversed treatment order
      result <- suppressWarnings({step_down_RSCABS(
        test_data,
        treatment_order = c("High", "Medium", "Low", "Control")
      )})

      # Check that the steps follow the specified order
      expect_equal(result$summary[[1]]$highest_treatment, "Control")
      expect_equal(result$summary[[2]]$highest_treatment, "Low")
      expect_equal(result$summary[[3]]$highest_treatment, "Medium")
    })


  it("validates input parameters", {
    # Missing treatment column
    expect_error(
      step_down_RSCABS(test_data, treatment_col = "treatment"),
      "Treatment column 'treatment' not found in data"
    )

    # Invalid treatment order
    expect_error(
      step_down_RSCABS(test_data, treatment_order = c("Control", "Low", "High")),
      "Some treatments in data are not in treatment_order"
    )

    expect_error(
      step_down_RSCABS(test_data, treatment_order = c("Control", "Low", "Medium", "High", "Extra")),
      "Some treatments in treatment_order are not in data"
    )

    # Only one treatment level
    single_treatment_data <- test_data[test_data$tmt == "Control", ]
    expect_error(
      step_down_RSCABS(single_treatment_data),
      "At least 2 treatment levels are required"
    )
  })
})

describe("print.StepDownRSCABS", {
  # Create a simple StepDownRSCABS object for testing
  test_object <- list(
    combined_results = data.frame(
      Threshold = rep(c("S1+", "S2+", "S3+"), 3),
      Z_statistic = rep(2:4, each = 3),
      P_value = c(0.03, 0.04, 0.06, 0.05, 0.07, 0.08, 0.1, 0.2, 0.3),
      Step = rep(1:3, each = 3),
      Included_Treatments = rep(c("Control, Low, Medium, High",
                                  "Control, Low, Medium",
                                  "Control, Low"), each = 3),
      Highest_Treatment = rep(c("High", "Medium", "Low"), each = 3)
    ),
    step_results = list(),
    summary = list(
      list(
        step = 1,
        included_treatments = "Control, Low, Medium, High",
        highest_treatment = "High",
        significant_thresholds = c("S1+", "S2+"),
        has_significant_findings = TRUE
      ),
      list(
        step = 2,
        included_treatments = "Control, Low, Medium",
        highest_treatment = "Medium",
        significant_thresholds = "S1+",
        has_significant_findings = TRUE
      ),
      list(
        step = 3,
        included_treatments = "Control, Low",
        highest_treatment = "Low",
        significant_thresholds = character(0),
        has_significant_findings = FALSE
      )
    ),
    lowest_significant = list(
      step = 2,
      treatment = "Medium",
      threshold = "S1+",
      p_value = 0.05
    ),
    parameters = list(
      treatment_col = "tmt",
      treatment_order = c("Control", "Low", "Medium", "High"),
      direction = "greater",
      alternative = "greater"
    )
  )
  class(test_object) <- "StepDownRSCABS"

  it("prints without error", {
    expect_output(print(test_object), "Step-Down RSCABS Analysis")
    expect_output(print(test_object,printLowest = T), "Lowest treatment level with significant findings")
    expect_output(print(test_object,printLowest = T), "Treatment: Medium")
  })
})

describe("summary.StepDownRSCABS", {
  # Create a simple StepDownRSCABS object for testing
  test_object <- list(
    combined_results = data.frame(
      Threshold = rep(c("S1+", "S2+", "S3+"), 3),
      Z_statistic = rep(2:4, each = 3),
      P_value = c(0.03, 0.04, 0.06, 0.05, 0.07, 0.08, 0.1, 0.2, 0.3),
      Step = rep(1:3, each = 3),
      Included_Treatments = rep(c("Control, Low, Medium, High",
                                  "Control, Low, Medium",
                                  "Control, Low"), each = 3),
      Highest_Treatment = rep(c("High", "Medium", "Low"), each = 3)
    ),
    step_results = list(),
    summary = list(
      list(
        step = 1,
        included_treatments = "Control, Low, Medium, High",
        highest_treatment = "High",
        significant_thresholds = c("S1+", "S2+"),
        has_significant_findings = TRUE
      ),
      list(
        step = 2,
        included_treatments = "Control, Low, Medium",
        highest_treatment = "Medium",
        significant_thresholds = "S1+",
        has_significant_findings = TRUE
      ),
      list(
        step = 3,
        included_treatments = "Control, Low",
        highest_treatment = "Low",
        significant_thresholds = character(0),
        has_significant_findings = FALSE
      )
    ),
    lowest_significant = list(
      step = 2,
      treatment = "Medium",
      threshold = "S1+",
      p_value = 0.05
    ),
    parameters = list(
      treatment_col = "tmt",
      treatment_order = c("Control", "Low", "Medium", "High"),
      direction = "greater",
      alternative = "greater"
    )
  )
  class(test_object) <- "StepDownRSCABS"

  it("returns a summary with the correct structure", {
    result <- summary(test_object)

    expect_true(is.list(result))
    expect_equal(names(result), c("summary_table", "lowest_significant", "parameters"))
    expect_equal(nrow(result$summary_table), 3)
    expect_equal(result$lowest_significant$treatment, "Medium")
  })
})

describe("plot.StepDownRSCABS", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")

  # Create a simple StepDownRSCABS object for testing
  test_object <- list(
    combined_results = data.frame(
      Threshold = rep(c("S1+", "S2+", "S3+"), 3),
      Z_statistic = rep(2:4, each = 3),
      P_value = c(0.03, 0.04, 0.06, 0.05, 0.07, 0.08, 0.1, 0.2, 0.3),
      Step = rep(1:3, each = 3),
      Included_Treatments = rep(c("Control, Low, Medium, High",
                                  "Control, Low, Medium",
                                  "Control, Low"), each = 3),
      Highest_Treatment = rep(c("High", "Medium", "Low"), each = 3)
    ),
    step_results = list(),
    summary = list(
      list(
        step = 1,
        included_treatments = "Control, Low, Medium, High",
        highest_treatment = "High",
        significant_thresholds = c("S1+", "S2+"),
        has_significant_findings = TRUE
      ),
      list(
        step = 2,
        included_treatments = "Control, Low, Medium",
        highest_treatment = "Medium",
        significant_thresholds = "S1+",
        has_significant_findings = TRUE
      ),
      list(
        step = 3,
        included_treatments = "Control, Low",
        highest_treatment = "Low",
        significant_thresholds = character(0),
        has_significant_findings = FALSE
      )
    ),
    lowest_significant = list(
      step = 2,
      treatment = "Medium",
      threshold = "S1+",
      p_value = 0.05
    ),
    parameters = list(
      treatment_col = "tmt",
      treatment_order = c("Control", "Low", "Medium", "High"),
      direction = "greater",
      alternative = "greater"
    )
  )
  class(test_object) <- "StepDownRSCABS"

  it("returns a ggplot object", {
    result <- plot(test_object)

    expect_true(inherits(result, "gg"))
    expect_true(inherits(result, "ggplot"))
  })
})



# Unit tests for RSCA threshold functions

# Test for run_threshold_RSCA
describe("run_threshold_RSCA", {
  # Create a sample dataset
  test_data <- data.frame(
    tmt = rep(c("Control", "Low", "Medium", "High"), each = 3),
    tank = paste0(rep(c("Control", "Low", "Medium", "High"), each = 3),
                  rep(1:3, times = 4)),
    S0 = c(8,7,9, 6,5,7, 4,5,3, 2,3,1),
    S1 = c(2,2,1, 3,4,2, 4,3,5, 4,3,5),
    S2 = c(0,1,0, 1,1,1, 2,2,1, 3,3,3),
    S3 = c(0,0,0, 0,0,0, 0,0,1, 1,1,1),
    S4 = c(0,0,0, 0,0,0, 0,0,0, 0,0,0),
    total = rep(10, 12)
  )

  # Dataset with zero counts
  zero_data <- data.frame(
    tmt = rep(c("Control", "Low", "High"), each = 3),
    tank = paste0(rep(c("Control", "Low", "High"), each = 3),
                  rep(1:3, times = 3)),
    S0 = c(10,10,10, 8,9,8, 5,6,5),
    S1 = c(0,0,0, 2,1,2, 3,2,3),
    S2 = c(0,0,0, 0,0,0, 2,2,2),
    total = rep(10, 9)
  )

  it("correctly calculates RSCA test for a valid threshold", {
    # Test S1+ threshold (all groups have non-zero counts)
    result <- run_threshold_RSCA(test_data, threshold = 1)

    # Check structure
    expect_equal(result$threshold, 1)
    expect_equal(result$threshold_label, "S1+")
    expect_false(is.na(result$Z))
    expect_false(is.na(result$p_value))

    expect_false(result$has_zero_counts) ## the run_threshold_RSCA function does not return has_zero_counts directly.

    # Check affected counts
    expect_equal(nrow(result$interm_values), 4)  # 4 treatment groups
    expect_true(all(result$interm_values$x > 0))  # All groups have affected fish
  })

  it("handles thresholds with zero counts", {
    # Test S3+ threshold (some groups have zero counts)
    expect_warning(
      result <- run_threshold_RSCA(test_data, threshold = 3),
      "Some treatment groups have zero affected individuals"
    )

    # Check that it flags zero counts
    expect_true(result$has_zero_counts)

    # Check affected counts
    expect_equal(sum(result$affected_counts$affected == 0), 2)  # Control and Low have zero S3+ fish
  })

  it("works with 'less than or equal to' direction", {
    # Test S≤1 threshold
    result <- run_threshold_RSCA(test_data, threshold = 1, direction = "less")

    # Check structure
    expect_equal(result$threshold_label, "S≤1")
    expect_false(is.na(result$Z))
    expect_false(is.na(result$p_value))

    # Check that counts make sense
    total_s0_s1 <- sum(test_data$S0) + sum(test_data$S1)
    expect_equal(sum(result$affected_counts$affected), total_s0_s1)
  })

  it("auto-detects score columns", {
    # Create data with non-standard column names but keep S columns
    modified_data <- test_data
    modified_data$treatment <- modified_data$tmt
    modified_data$replicate <- modified_data$tank
    modified_data$count <- modified_data$total
    modified_data$tmt <- NULL
    modified_data$tank <- NULL
    modified_data$total <- NULL

    # Should still find S0-S4 columns
    result <- run_threshold_RSCA(
      modified_data,
      threshold = 1,
      treatment_col = "treatment",
      replicate_col = "replicate",
      total_col = "count"
    )

    expect_false(is.na(result$Z))
  })

  it("raises error with invalid inputs", {
    # Missing treatment column
    expect_error(
      run_threshold_RSCA(test_data, threshold = 1, treatment_col = "treatment"),
      "Treatment column 'treatment' not found in data"
    )

    # Invalid direction
    expect_error(
      run_threshold_RSCA(test_data, threshold = 1, direction = "invalid"),
      "Direction must be either 'greater' or 'less'"
    )

    # Invalid score columns
    expect_error(
      run_threshold_RSCA(test_data, threshold = 1, score_cols = c("S0", "S1", "S99")),
      "Missing score columns"
    )
  })
})

# Test for run_all_threshold_tests
describe("run_all_threshold_tests", {
  # Create a sample dataset
  test_data <- data.frame(
    tmt = rep(c("Control", "Low", "Medium", "High"), each = 3),
    tank = paste0(rep(c("Control", "Low", "Medium", "High"), each = 3),
                  rep(1:3, times = 4)),
    S0 = c(8,7,9, 6,5,7, 4,5,3, 2,3,1),
    S1 = c(2,2,1, 3,4,2, 4,3,5, 4,3,5),
    S2 = c(0,1,0, 1,1,1, 2,2,1, 3,3,3),
    S3 = c(0,0,0, 0,0,0, 0,0,1, 1,1,1),
    S4 = c(0,0,0, 0,0,0, 0,0,0, 0,0,0),
    total = rep(10, 12)
  )

  it("runs tests for all thresholds", {
    # Run tests for S1+ through S4+
    result <- suppressWarnings(run_all_threshold_tests(test_data)) ## warning is expected

    # Check structure
    expect_equal(nrow(result$results), 4)  # 4 thresholds (S1+ through S4+)
    expect_equal(result$results$Threshold, c("S1+", "S2+", "S3+", "S4+"))

    # Check that proportions table has correct structure
    expect_equal(nrow(result$proportions), 4)  # 4 treatment groups
    expect_equal(ncol(result$proportions), 5)  # Treatment + 4 thresholds

    # Check that detailed results are included
    expect_equal(length(result$detailed_results), 4)
  })

  it("handles custom min and max scores", {
    # Run tests only for S2+ and S3+
    result <- suppressWarnings(run_all_threshold_tests(test_data, min_score = 2, max_score = 3))

    # Check structure
    expect_equal(nrow(result$results), 2)  # 2 thresholds
    expect_equal(result$results$Threshold, c("S2+", "S3+"))
  })

  it("works with 'less than or equal to' direction", {
    # Run tests for S≤0 through S≤4
    result <- run_all_threshold_tests(
      test_data,
      min_score = 0,
      max_score = 4,
      direction = "less"
    )

    # Check structure
    expect_equal(nrow(result$results), 5)  # 5 thresholds
    expect_equal(result$results$Threshold, c("S≤0", "S≤1", "S≤2", "S≤3", "S≤4"))

    # Check proportions make sense
    # S≤4 should be 1.0 for all treatments (all fish have S≤4)
    expect_equal(unique(result$proportions$`S≤4`), 1)
  })

  it("includes Fisher's exact test for thresholds with zero counts", {
    # Run with Fisher's test for thresholds with zero counts
    result <- suppressWarnings(run_all_threshold_tests(test_data, include_fisher = TRUE))

    # Check that methods are assigned correctly
    expect_true("Method" %in% names(result$results))
    expect_true(any( grepl("RSCA",result$results$Method)))

    # For thresholds with zero counts, should use Fisher's test or mark as not calculable
    zero_count_rows <- which(result$results$Has_zero_counts)
    if (length(zero_count_rows) > 0) {
      expect_true(all(grepl("Fisher's Exact Test",result$results$Method[zero_count_rows]) |
                        grepl("Not calculable",result$results$Method[zero_count_rows])) )
    }
  })

  it("auto-detects max score", {
    # Create data with only S0-S3 columns
    modified_data <- test_data[, c("tmt", "tank", "S0", "S1", "S2", "S3", "total")]

    # Should detect max score of 3
    result <- suppressWarnings(run_all_threshold_tests(modified_data))

    # Check structure
    expect_equal(nrow(result$results), 3)  # 3 thresholds (S1+ through S3+)
    expect_equal(result$results$Threshold, c("S1+", "S2+", "S3+"))
  })
})

describe("RSCABS",{
  # Mock data for testing
  set.seed(123)
  mock_data <- data.frame(
    Treatment = factor(rep(c("Control", "Dose1", "Dose2"), each = 10)),
    Replicate = rep(1:10, 3),
    Endpoint1 = c(rpois(10, 2), rpois(10, 3), rpois(10,5)),
    Endpoint2 = c(rpois(10, 1), rpois(10, 2), rpois(10, 3))
  )
  #mock_data <- pvi_example %>% mutate(y0 = as.numeric(factor(y0)))
  # Unit tests
  it("runRSCABS calculates correctly", {
    result <- runRSCABS(as.data.frame(mock_data), "Treatment", "Replicate",Effects = "Endpoint1")

    expect_s3_class(result, "data.frame")
    expect_true("Statistic" %in% colnames(result))
    expect_true("P.Value" %in% colnames(result))
    expect_identical(result$Signif,c(".", ".", "**", ".", "**", ".", "***", "*", "**", ".", "*",
                                     "*", ".", ".", "."))
  })

  it("RSCABK calculates correctly for RS type", {
    x.i.j <- matrix(c(5, 3, 4, 2), nrow = 2)
    n.i.j <- matrix(c(10, 10, 10, 10), nrow = 2)
    m.i <- c(2, 2)
    TestK <- 1
    test.type <- "RS"

    result <- RSCABK(x.i.j, n.i.j, m.i, TestK, test.type)

    expect_true("Treatment" %in% names(result))
    expect_true("Statistic" %in% names(result))
    expect_true("P-Value" %in% names(result))
  })

  it("prepDataRSCABS prepares data correctly", {
    mock_data$Replicate <- factor(mock_data$Replicate) ## need Replicate to be changed to factor!!!
    result <- prepDataRSCABS(Effect = "Endpoint1", Data = mock_data, "Treatment", "Replicate")

    expect_true("x.i.j" %in% names(result))
    expect_true("n.i.j" %in% names(result))
    expect_true("m.i" %in% names(result))
  })

  it("convert2Score handles negative values", {
    result <- convert2Score(c(5, -1, 3, 0, NA))

    expect_equal(result, c(5, NA, 3, 0, NA))
  })

})





