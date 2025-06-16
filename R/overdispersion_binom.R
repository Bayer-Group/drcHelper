#' Tarone's test for overdispersion accounting for trend
#' @param successes Vector of success counts
#' @param totals Vector of total trials
#' @param doses Vector of dose levels
#' @param scoring Method for defining trend: "doses", "ranks", "log_doses", "equal_spaced"
#' @param include_groupwise_tarone Logical, whether to include groupwise Tarone tests
#' @return List containing test results
#' @export
Tarone.trend.test <- function(successes, totals, doses,
                              scoring = c("doses", "ranks", "log_doses", "equal_spaced"),
                              include_groupwise_tarone = TRUE) {

  # Input validation
  if (length(successes) != length(totals) || length(successes) != length(doses)) {
    stop("'successes', 'totals', and 'doses' must have the same length")
  }

  if (any(successes > totals)) {
    stop("'successes' cannot be greater than 'totals'")
  }

  scoring <- match.arg(scoring)

  # Remove incomplete cases
  OK <- complete.cases(successes, totals, doses)
  succ_raw <- successes[OK]
  tots_raw <- totals[OK]
  dose_raw <- doses[OK]

  # Create data frame for analysis
  raw_data <- data.frame(
    successes = succ_raw,
    totals = tots_raw,
    doses = dose_raw
  )

  # Summarize by dose for trend analysis
  dose_summary <- raw_data %>%
    dplyr::group_by(doses) %>%
    dplyr::summarise(
      successes = sum(successes),
      totals = sum(totals),
      n_replicates = dplyr::n(),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(doses)

  # Extract vectors for trend analysis
  succ <- dose_summary$successes
  tots <- dose_summary$totals
  dose_vals <- dose_summary$doses
  n_groups <- length(dose_vals)

  if (n_groups < 2) stop("At least two dose groups required")

  # Calculate scores based on method
  scores <- switch(scoring,
                   "doses" = dose_vals,
                   "ranks" = seq_along(dose_vals),
                   "log_doses" = {
                     if (any(dose_vals <= 0)) {
                       warning("Non-positive doses found, using log(dose + 1)")
                       log(dose_vals + 1)
                     } else {
                       log(dose_vals)
                     }
                   },
                   "equal_spaced" = seq(0, 1, length.out = n_groups)
  )

  # Results list
  results <- list(
    scoring_method = scoring,
    doses = dose_vals,
    scores = scores,
    dose_summary = dose_summary
  )

  # Groupwise Tarone tests (within each dose group)
  if (include_groupwise_tarone) {
    groupwise_tests <- perform_groupwise_tarone(raw_data)
    results$groupwise_tarone <- groupwise_tests
  }

  # Trend-adjusted Tarone test and phi estimation
  trend_results <- tarone_with_trend_removal(succ, tots, scores)
  results$trend_adjusted_tarone <- trend_results$test
  results$phi_estimate <- trend_results$phi
  results$trend_fit <- trend_results$trend_fit

  class(results) <- "TaroneTrendTest"
  return(results)
}



#' Perform Tarone test within each dose group
#' @param data Data frame with successes, totals, doses
perform_groupwise_tarone <- function(data) {

  # Group by dose and perform Tarone test for each group with replicates
  groupwise_results <- data %>%
    dplyr::group_by(doses) %>%
    dplyr::summarise(
      n_replicates = dplyr::n(),
      tarone_result = list({
        if (dplyr::n() > 1) {  # Only test if there are replicates
          tryCatch({
            Tarone.test(totals, successes)
          }, error = function(e) {
            list(
              statistic = NA,
              p.value = NA,
              method = "Tarone test (failed)",
              error = e$message
            )
          })
        } else {
          list(
            statistic = NA,
            p.value = NA,
            method = "Tarone test (single replicate)",
            note = "Cannot test overdispersion with single replicate"
          )
        }
      }),
      .groups = 'drop'
    )

  # Extract results into a cleaner format
  tarone_summary <- data.frame(
    dose = groupwise_results$doses,
    n_replicates = groupwise_results$n_replicates,
    z_statistic = sapply(groupwise_results$tarone_result, function(x) x$statistic %||% NA),
    p_value = sapply(groupwise_results$tarone_result, function(x) x$p.value %||% NA),
    significant = sapply(groupwise_results$tarone_result, function(x) {
      if (is.na(x$p.value %||% NA)) return(NA)
      x$p.value < 0.05
    }),
    note = sapply(groupwise_results$tarone_result, function(x) x$note %||% "")
  )

  return(list(
    summary = tarone_summary,
    detailed_results = groupwise_results$tarone_result
  ))
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x



#' Internal function to perform Tarone test with trend removal and calculate phi
#' @param successes Vector of success counts (summarized by dose)
#' @param totals Vector of total trials (summarized by dose)
#' @param scores Vector of scores defining the trend
tarone_with_trend_removal <- function(successes, totals, scores) {

  # Calculate overall proportion
  p_overall <- sum(successes) / sum(totals)

  # Calculate trend component
  mean_score <- sum(totals * scores) / sum(totals)

  # Fit linear trend model
  # Expected proportion under linear trend: p_i = p_overall + beta * (score_i - mean_score)
  numerator <- sum(totals * (scores - mean_score) * (successes/totals - p_overall))
  denominator <- sum(totals * (scores - mean_score)^2)

  if (denominator > 0) {
    beta <- numerator / denominator
    trend_proportions <- p_overall + beta * (scores - mean_score)
  } else {
    beta <- 0
    trend_proportions <- rep(p_overall, length(scores))
  }

  # Ensure proportions are within bounds
  trend_proportions <- pmax(0.001, pmin(0.999, trend_proportions))

  # Calculate expected counts under trend model
  expected_trend <- totals * trend_proportions

  # Calculate residuals after trend removal
  residuals <- successes - expected_trend

  # Calculate Tarone statistic on trend-adjusted residuals
  variance_components <- expected_trend * (1 - trend_proportions)
  S_adj <- sum(residuals^2 / variance_components)

  # Degrees of freedom adjusted for trend fitting
  df_adj <- length(totals) - 2  # -1 for overall mean, -1 for trend parameter

  # Test statistic
  if (df_adj > 0) {
    z_stat <- (S_adj - df_adj) / sqrt(2 * df_adj)
    p_value <- 2 * pnorm(-abs(z_stat))
  } else {
    z_stat <- NA
    p_value <- NA
    warning("Insufficient degrees of freedom for trend-adjusted test")
  }

  # Calculate phi from trend-adjusted residuals (CORRECTED)
  if (df_adj > 0) {
    phi_estimate <- S_adj / df_adj
    phi_estimate <- max(1, phi_estimate)  # Ensure phi >= 1
  } else {
    phi_estimate <- 1
  }

  # Create result object
  test_result <- list(
    statistic = z_stat,
    p.value = p_value,
    method = "Tarone's test for overdispersion (trend-adjusted)",
    alternative = "two.sided",
    data.name = paste("successes from totals with trend removed")
  )
  class(test_result) <- "htest"

  # Trend fitting information
  trend_fit <- list(
    beta = beta,
    trend_proportions = trend_proportions,
    expected_under_trend = expected_trend,
    residuals_after_trend = residuals,
    variance_components = variance_components
  )

  return(list(
    test = test_result,
    phi = phi_estimate,
    trend_fit = trend_fit
  ))
}

#' Print method for TaroneTrendTest with tabular output
#' @param x TaroneTrendTest object
#' @param ... Additional arguments
print.TaroneTrendTest <- function(x, ...) {
  cat("Tarone Overdispersion Test with Trend Analysis\n")
  cat("==============================================\n\n")

  # Summary information
  cat("Analysis Settings:\n")
  settings_df <- data.frame(
    Setting = c("Scoring Method", "Number of Dose Groups", "Total Observations"),
    Value = c(x$scoring_method, length(x$doses), sum(x$dose_summary$totals))
  )
  print(settings_df, row.names = FALSE)
  cat("\n")

  # Dose and scoring table
  cat("Dose-Score Mapping:\n")
  dose_score_df <- data.frame(
    Dose = x$doses,
    Score = round(x$scores, 3),
    Successes = x$dose_summary$successes,
    Totals = x$dose_summary$totals,
    Proportion = round(x$dose_summary$successes / x$dose_summary$totals, 3),
    Replicates = x$dose_summary$n_replicates
  )
  print(dose_score_df, row.names = FALSE)
  cat("\n")

  # Groupwise Tarone test results table
  if (!is.null(x$groupwise_tarone)) {
    cat("Groupwise Tarone Tests (Within-Dose Overdispersion):\n")
    groupwise_df <- x$groupwise_tarone$summary %>%
      dplyr::mutate(
        Z_Statistic = ifelse(is.nan(z_statistic), "NaN (no variation)",
                             sprintf("%.3f", z_statistic)),
        P_Value = ifelse(is.nan(p_value), "NA", sprintf("%.4f", p_value)),
        Result = dplyr::case_when(
          is.na(significant) ~ "Cannot test",
          significant ~ "Overdispersed *",
          !significant ~ "Not overdispersed",
          TRUE ~ "Unknown"
        )
      ) %>%
      dplyr::select(
        Dose = dose,
        N_Replicates = n_replicates,
        Z_Statistic,
        P_Value,
        Result
      )

    print(groupwise_df, row.names = FALSE)
    cat("\n")
  }

  # Trend analysis results table
  cat("Trend-Adjusted Analysis:\n")
  trend_df <- data.frame(
    Test = c("Trend-Adjusted Tarone", "Overdispersion Parameter"),
    Statistic = c(
      ifelse(is.na(x$trend_adjusted_tarone$statistic), "NA",
             sprintf("Z = %.3f", x$trend_adjusted_tarone$statistic)),
      sprintf("φ = %.3f", x$phi_estimate)
    ),
    P_Value = c(
      ifelse(is.na(x$trend_adjusted_tarone$p.value), "NA",
             sprintf("%.4f", x$trend_adjusted_tarone$p.value)),
      ifelse(x$phi_estimate > 1.2, "Suggests overdispersion", "Within normal range")
    ),
    Interpretation = c(
      ifelse(is.na(x$trend_adjusted_tarone$p.value), "Cannot test",
             ifelse(x$trend_adjusted_tarone$p.value < 0.05,
                    "Significant overdispersion *", "No overdispersion")),
      ifelse(x$phi_estimate > 1.2, "Use Rao-Scott correction", "Standard analysis OK")
    )
  )
  print(trend_df, row.names = FALSE)
  cat("\n")

  # Trend fitting details
  if (!is.null(x$trend_fit)) {
    cat("Trend Model Details:\n")
    trend_details_df <- data.frame(
      Dose = x$doses,
      Observed_Prop = round(x$dose_summary$successes / x$dose_summary$totals, 4),
      Expected_Prop = round(x$trend_fit$trend_proportions, 4),
      Expected_Count = round(x$trend_fit$expected_under_trend, 2),
      Residual = round(x$trend_fit$residuals_after_trend, 2)
    )
    print(trend_details_df, row.names = FALSE)
  }

  invisible(x)
}


#' Compare Tarone trend tests across different scoring methods
#' @param successes Vector of success counts
#' @param totals Vector of total trials
#' @param doses Vector of dose levels
#' @param scoring_methods Vector of scoring methods to compare
#' @return List of results with comparison table
compare_tarone_scoring <- function(successes, totals, doses,
                                   scoring_methods = c("doses", "ranks", "log_doses", "equal_spaced")) {

  # Run tests for each scoring method
  results <- list()
  for (method in scoring_methods) {
    cat(sprintf("\n=== Running analysis with %s scoring ===\n", method))
    results[[method]] <- Tarone.trend.test(
      successes = successes,
      totals = totals,
      doses = doses,
      scoring = method,
      include_groupwise_tarone = TRUE
    )
  }

  # Create comparison table
  comparison_df <- data.frame(
    Scoring_Method = scoring_methods,
    Phi_Estimate = sapply(results, function(x) round(x$phi_estimate, 3)),
    Trend_Z_Stat = sapply(results, function(x) {
      if (is.na(x$trend_adjusted_tarone$statistic)) "NA"
      else sprintf("%.3f", x$trend_adjusted_tarone$statistic)
    }),
    Trend_P_Value = sapply(results, function(x) {
      if (is.na(x$trend_adjusted_tarone$p.value)) "NA"
      else sprintf("%.4f", x$trend_adjusted_tarone$p.value)
    }),
    Overdispersion = sapply(results, function(x) {
      if (is.na(x$trend_adjusted_tarone$p.value)) "Cannot test"
      else ifelse(x$trend_adjusted_tarone$p.value < 0.05, "Yes *", "No")
    }),
    Recommendation = sapply(results, function(x) {
      if (x$phi_estimate > 1.5) "Strong Rao-Scott correction needed"
      else if (x$phi_estimate > 1.2) "Rao-Scott correction recommended"
      else "Standard analysis appropriate"
    })
  )

  cat("\n", rep_char("=", 80), "\n", sep = "")
  cat("COMPARISON ACROSS SCORING METHODS\n")
  cat(rep_char("=", 80), "\n", sep = "")
  print(comparison_df, row.names = FALSE)

  # Summary of groupwise tests (doses with potential overdispersion)
  cat("\nGroupwise Overdispersion Summary:\n")
  groupwise_summary <- data.frame(
    Dose = results[[1]]$doses,
    N_Replicates = results[[1]]$dose_summary$n_replicates,
    Proportion = round(results[[1]]$dose_summary$successes / results[[1]]$dose_summary$totals, 3),
    Tarone_P_Value = sapply(results[[1]]$groupwise_tarone$summary$p_value, function(p) {
      if (is.na(p) || is.nan(p)) "NA" else sprintf("%.4f", p)
    }),
    Overdispersed = sapply(results[[1]]$groupwise_tarone$summary$significant, function(s) {
      if (is.na(s)) "Cannot test" else ifelse(s, "Yes *", "No")
    })
  )
  print(groupwise_summary, row.names = FALSE)

  return(list(
    individual_results = results,
    comparison_table = comparison_df,
    groupwise_summary = groupwise_summary
  ))
}

# Helper for string concatenation and repetition
`%+%` <- function(x, y) paste0(x, y)
rep_char <- function(char, n) paste(rep(char, n), collapse = "")


#' Create a summary table for manuscript/report
#' @param comparison_results Output from compare_tarone_scoring
create_summary_table <- function(comparison_results) {

  # Main results table
  main_table <- comparison_results$comparison_table %>%
    dplyr::mutate(
      `Scoring Method` = Scoring_Method,
      `φ (Overdispersion)` = Phi_Estimate,
      `Trend Test Z` = Trend_Z_Stat,
      `P-value` = Trend_P_Value,
      `Significant?` = Overdispersion,
      `Analysis Recommendation` = Recommendation
    ) %>%
    dplyr::select(-Scoring_Method, -Phi_Estimate, -Trend_Z_Stat,
                  -Trend_P_Value, -Overdispersion, -Recommendation)

  cat("Summary Table for Publication:\n")
  cat("==============================\n")
  print(main_table, row.names = FALSE)

  # Interpretation
  cat("\nInterpretation Guide:\n")
  cat("- φ > 1.2: Consider overdispersion correction\n")
  cat("- φ > 1.5: Strong evidence of overdispersion\n")
  cat("- * indicates statistical significance (p < 0.05)\n")
  cat("- Groupwise tests examine within-dose variation\n")
  cat("- Trend-adjusted tests examine overdispersion after removing dose-response trend\n")

  return(main_table)
}
