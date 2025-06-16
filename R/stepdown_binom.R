#' Cochran-Armitage Trend Test for Binomial Data
#'
#' Performs a Cochran-Armitage test for trend in binomial proportions across
#' ordered groups, with an optional Rao-Scott correction for overdispersion.
#'
#' @param successes Numeric vector of successes (e.g., number of survivors).
#' @param totals Numeric vector of total observations (e.g., total organisms).
#' @param doses Numeric vector of dose levels or scores corresponding to each observation.
#' @param scoring Method for defining trend
#' @param alternative Character string specifying the alternative hypothesis.
#'        Must be one of "two.sided" (default), "greater", or "less".
#' @param rao_scott Logical indicating whether to apply Rao-Scott correction for
#'        overdispersion (default: FALSE).
#' @param phi Overdispersion parameter for Rao-Scott correction. If NULL (default),
#'        it will be estimated from the data.
#' @param phi_method simple without trend adjustment or trend_adjusted.
#' @return A list containing:
#'   \item{statistic}{Test statistic (Z score)}
#'   \item{p.value}{P-value for the test}
#'   \item{method}{Description of the test performed}
#'   \item{alternative}{The alternative hypothesis}
#'   \item{data.name}{Description of the data}
#'   \item{phi}{Overdispersion parameter (only if rao_scott = TRUE)}
#'
#' @details
#' The Cochran-Armitage test is used to detect trends in binomial proportions
#' across ordered groups. The test statistic follows a standard normal distribution
#' under the null hypothesis of no trend.
#'
#' When rao_scott = TRUE, the function applies a correction for overdispersion,
#' which is often present in clustered binomial data. The correction divides the
#' test statistic by the square root of the estimated dispersion parameter.
#'
#' @examples
#' # Example with simulated data
#' successes <- c(20, 18, 15, 10, 5)
#' totals <- rep(20, 5)
#' doses <- c(0, 1, 2, 5, 10)
#'
#' # Standard Cochran-Armitage test
#' result <- cochranArmitageTrendTest(successes, totals, doses)
#' print(result)
#'
#' # With Rao-Scott correction
#' result_rs <- cochranArmitageTrendTest(successes, totals, doses, rao_scott = TRUE)
#' print(result_rs)
#'
#' @importFrom stats pnorm complete.cases
#' @importFrom dplyr group_by summarise ungroup
#' @export
cochranArmitageTrendTest <- function(successes, totals, doses,
                                     scoring = c("doses", "ranks", "log_doses", "equal_spaced"),
                                     alternative = c("two.sided", "greater", "less"),
                                     rao_scott = FALSE, phi = NULL,
                                     phi_method = c("simple", "trend_adjusted")) {

  # Match arguments
  alternative <- match.arg(alternative)
  scoring <- match.arg(scoring)
  phi_method <- match.arg(phi_method)

  # Check inputs
  if (length(successes) != length(totals) || length(successes) != length(doses)) {
    stop("'successes', 'totals', and 'doses' must have the same length")
  }

  if (any(successes > totals)) {
    stop("'successes' cannot be greater than 'totals'")
  }

  # Remove incomplete cases and summarize by dose
  OK <- complete.cases(successes, totals, doses)
  dat <- data.frame(successes = successes[OK], totals = totals[OK], doses = doses[OK]) %>%
    dplyr::group_by(doses) %>%
    dplyr::summarise(successes = sum(successes), totals = sum(totals), .groups = 'drop')

  successes <- dat$successes
  totals <- dat$totals
  doses <- dat$doses

  n <- length(successes)
  if (n < 2) stop("At least two groups required")

  # Calculate proportions
  p <- successes / totals

  # Calculate scores based on method
  scores <- switch(scoring,
                   "doses" = doses,
                   "ranks" = seq_along(doses),
                   "log_doses" = {
                     if (any(doses <= 0)) {
                       warning("Non-positive doses found, using log(dose + 1)")
                       log(doses + 1)
                     } else {
                       log(doses)
                     }
                   },
                   "equal_spaced" = seq(0, 1, length.out = n)
  )

  # Calculate mean score
  mean_score <- sum(totals * scores) / sum(totals)

  # Calculate numerator
  numerator <- sum(totals * (scores - mean_score) * p)

  # Calculate denominator components
  p_overall <- sum(successes) / sum(totals)
  q_overall <- 1 - p_overall

  # Standard denominator for Cochran-Armitage
  denominator <- sqrt(p_overall * q_overall * sum(totals * (scores - mean_score)^2))

  # Calculate test statistic
  if (denominator > 0) {
    z_stat <- numerator / denominator
  } else {
    z_stat <- 0
    warning("Denominator is zero, test statistic set to zero")
  }

  # Apply Rao-Scott correction if requested
  # Apply Rao-Scott correction if requested
  rs_phi <- 1  # Default (no correction)
  if (rao_scott) {
    if (is.null(phi)) {
      # Estimate phi using specified method
      rs_phi <- estimate_phi_with_scoring(successes, totals, doses, scoring, phi_method)
      cat("Estimated phi =", round(rs_phi, 3), "using", phi_method, "method with", scoring, "scoring\n")
    } else {
      rs_phi <- phi
    }
    rs_phi <- max(1, rs_phi)  # Ensure phi >= 1

    # Adjust test statistic
    z_stat <- z_stat / sqrt(rs_phi)
  }

  # Calculate p-value based on alternative
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z_stat))
  } else if (alternative == "greater") {
    p_value <- pnorm(-z_stat)
  } else {  # less
    p_value <- pnorm(z_stat)
  }

  # Prepare output
  method_name <- if(rao_scott) {
    paste("Rao-Scott corrected Cochran-Armitage test for trend (", scoring, " scoring)", sep = "")
  } else {
    paste("Cochran-Armitage test for trend (", scoring, " scoring)", sep = "")
  }

  data_name <- paste("proportions", paste(round(p, 3), collapse = ", "),
                     "at doses", paste(doses, collapse = ", "))

  result <- list(
    statistic = z_stat,
    p.value = p_value,
    method = method_name,
    alternative = alternative,
    data.name = data_name,
    scoring = scoring,
    scores = scores
  )

  if (rao_scott) {
    result$phi <- rs_phi
  }

  class(result) <- c("htest", "cochranArmitage")
  return(result)
}

#' Step-down Cochran-Armitage trend test with consistent scoring
#' @param successes Vector of success counts
#' @param totals Vector of total trials
#' @param doses Vector of dose levels
#' @param scoring Method for defining trend: "doses", "ranks", "log_doses", "equal_spaced"
#' @param alternative Direction of test
#' @param rao_scott Whether to apply Rao-Scott correction
#' @param phi Overdispersion parameter (if NULL, estimated from Tarone test)
#' @param phi_method simple or trend_adjusted description
#' @param tarone_results Optional results from Tarone.trend.test for phi estimation
#' @return stepDownTrendBinom object with consistent scoring A list with class "stepDownTrendBinom"
#' containing:
#'   \item{method}{Description of the test performed}
#'   \item{data.name}{Description of the data}
#'   \item{p.value}{Matrix of p-values for each step}
#'   \item{statistic}{Matrix of test statistics for each step}
#'   \item{alternative}{The alternative hypothesis}
#'   \item{doses}{The dose levels used}
#'   \item{noec}{The No Observed Effect Concentration (highest dose with no significant effect)}
#'   \item{loec}{The Lowest Observed Effect Concentration (lowest dose with significant effect)}
#'
#' @details
#' The step-down procedure starts with all dose groups and sequentially removes the
#' highest dose until no significant trend is detected or only two groups remain.
#' This helps identify the lowest dose at which a significant effect occurs.
#'
#' The Cochran-Armitage test is appropriate for detecting trends in binomial proportions
#' across ordered groups. The Rao-Scott correction adjusts for potential overdispersion
#' in binomial data, which is common in toxicological studies.
#'
#' @examples
#' # Example with simulated data
#' successes <- c(20, 18, 15, 10, 5)
#' totals <- rep(20, 5)
#' doses <- c(0, 1, 2, 5, 10)
#'
#' # Run step-down test with Cochran-Armitage
#' result <- stepDownTrendTestBinom(successes, totals, doses)
#' print(result)
#'
#' # Run with Rao-Scott correction
#' result_rs <- stepDownTrendTestBinom(successes, totals, doses, rao_scott = TRUE)
#' print(result_rs)
#'
#' @importFrom stats complete.cases
#' @export
stepDownTrendTestBinom <- function(successes, totals, doses,
                                   scoring = c("doses", "ranks", "log_doses", "equal_spaced"),
                                   alternative = c("two.sided", "greater", "less"),
                                   rao_scott = FALSE, phi = NULL,
                                   phi_method = c("simple", "trend_adjusted"),
                                   tarone_results = NULL) {

  # Match arguments
  alternative <- match.arg(alternative)
  scoring <- match.arg(scoring)
  phi_method <- match.arg(phi_method)
  # Check inputs
  if (length(successes) != length(totals) || length(successes) != length(doses)) {
    stop("'successes', 'totals', and 'doses' must have the same length")
  }

  if (any(successes > totals)) {
    stop("'successes' cannot be greater than 'totals'")
  }

  # Remove incomplete cases and summarize by dose
  OK <- complete.cases(successes, totals, doses)
  dat <- data.frame(
    successes = successes[OK],
    totals = totals[OK],
    doses = doses[OK]
  ) %>%
    dplyr::group_by(doses) %>%
    dplyr::summarise(
      successes = sum(successes),
      totals = sum(totals),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(doses)

  # Extract vectors
  succ <- dat$successes
  tots <- dat$totals
  dose_vals <- dat$doses

  # Order data by doses
  ord <- order(dose_vals)
  succ <- succ[ord]
  tots <- tots[ord]
  dose_vals <- dose_vals[ord]

  # Number of dose groups
  dose_levels <- unique(dose_vals)
  k <- length(dose_levels)
  if (k < 2) {
    stop("at least two dose groups are required")
  }

  # Get phi estimate if using Rao-Scott correction
  phi_estimate <- 1  # Default
  phi_source <- "none"

  if (rao_scott) {
    if (!is.null(phi)) {
      phi_estimate <- phi
      phi_source <- "user_provided"
    } else if (!is.null(tarone_results)) {
      phi_estimate <- tarone_results$phi_estimate
      phi_source <- "tarone_trend_adjusted"
      cat("Using phi =", round(phi_estimate, 3), "from Tarone trend test\n")
    } else {
      # Estimate phi using specified method
      phi_estimate <- estimate_phi_with_scoring(succ, tots, dose_vals, scoring, phi_method)
      phi_source <- paste0("estimated_", phi_method)
      cat("Estimated phi =", round(phi_estimate, 3), "using", phi_method, "method with", scoring, "scoring\n")
    }
  }

  # Data name for output
  DNAME <- paste("successes out of totals at doses", paste(dose_vals, collapse = ", "),
                 "with", scoring, "scoring")

  # Perform step-down procedure
  results <- list()
  for (i in 1:(k-1)) {
    # Select groups from 1 to k-i+1 (removing highest doses first)
    idx_levels <- 1:(k-i+1)
    idx <- which(dose_vals %in% dose_levels[idx_levels])

    # Run test on selected groups with consistent scoring
    test_result <- cochranArmitageTrendTest(
      successes = succ[idx],
      totals = tots[idx],
      doses = dose_vals[idx],
      scoring = scoring,  # Pass scoring method
      alternative = alternative,
      rao_scott = rao_scott,
      phi = phi_estimate,
      phi_method = phi_method
    )

    results[[i]] <- test_result
  }

  # Extract p-values and statistics
  pvals <- sapply(results, function(x) x$p.value)
  stats <- sapply(results, function(x) x$statistic)

  # Create matrices for output
  PVAL <- matrix(pvals, nrow = k-1, ncol = 1)
  STAT <- matrix(stats, nrow = k-1, ncol = 1)

  # Set row and column names
  rownames(PVAL) <- rownames(STAT) <- paste("Step", 1:(k-1))
  colnames(PVAL) <- colnames(STAT) <- "Value"

  # Determine NOEC and LOEC
  sig_idx <- which(pvals > 0.05)[1]

  if (is.na(sig_idx)) {
    # All tests significant
    noec <- NA
    loec <- dose_levels[1]  # Lowest dose
  } else if (sig_idx == 1) {
    # No significant trend at all
    noec <- dose_levels[k]  # Highest dose
    loec <- NA
  } else {
    # Some significant results
    noec <- dose_levels[k - sig_idx + 1]
    loec <- dose_levels[k - sig_idx + 2]
  }

  # Create output object
  METHOD <- paste0("Step-down ", results[[1]]$method, " (", scoring, " scoring)")

  ans <- list(
    method = METHOD,
    data.name = DNAME,
    p.value = PVAL,
    statistic = STAT,
    alternative = alternative,
    doses = dose_vals,
    scoring = scoring,
    phi = phi_estimate,
    phi_method = if(rao_scott) phi_method else NULL,
    phi_source = phi_source,
    rao_scott = rao_scott,
    noec = noec,
    loec = loec,
    step_results = results
  )

  class(ans) <- "stepDownTrendBinom"
  return(ans)
}

#' Print Method for stepDownTrendBinom Objects
#'
#' @param x An object of class "stepDownTrendBinom"
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object
#' @export
print.stepDownTrendBinom <- function(x, ...) {
  cat("\n")
  cat(x$method, "\n\n")
  cat("data:", x$data.name, "\n\n")
  cat("Step-down results:\n")

  # Create a data frame for printing
  results_df <- data.frame(
    Doses_Included = sapply(1:(length(x$doses)-1), function(i) {
      paste(x$doses[1:(length(x$doses)-i+1)], collapse = ", ")
    }),
    Statistic = x$statistic[,1],
    P_Value = x$p.value[,1]
  )

  print(results_df, ...)

  cat("\nAlternative hypothesis:", x$alternative, "\n")

  if (!is.na(x$noec)) {
    cat("NOEC:", x$noec, "\n")
  } else {
    cat("NOEC: Not determined (all doses show significant effects)\n")
  }

  if (!is.na(x$loec)) {
    cat("LOEC:", x$loec, "\n")
  } else {
    cat("LOEC: Not determined (no significant effects detected)\n")
  }

  invisible(x)
}



#' Function to test for overdispersion in binomial data
#'
#' @param successes Numeric vector of successes (e.g., number of survivors).
#' @param totals Numeric vector of total observations (e.g., total organisms).
#' @param model A fitted binomial GLM with predictors accounted for. (default is NULL)
#'
#' @return a list of dispersion test results
#' @export
test_overdispersion <- function(successes, totals, model = NULL) {
  if (is.null(model)) {
    # If no model provided, fit a simple binomial GLM
    # (You might want to include predictors relevant to your study)
    model <- glm(cbind(successes, totals - successes) ~ 1,
                 family = binomial())
  }

  # Calculate Pearson residuals
  pearson_resid <- residuals(model, type = "pearson")

  # Calculate dispersion parameter
  phi <- sum(pearson_resid^2) / model$df.residual

  # Conduct a formal test using the fact that sum(pearson_resid^2) ~ χ²(df)
  p_value <- 1 - pchisq(sum(pearson_resid^2), df = model$df.residual)

  return(list(
    dispersion = phi,
    p_value = p_value,
    interpretation = ifelse(phi > 1, "Overdispersion detected", "No significant overdispersion")
  ))
}


#' Estimate overdispersion parameter using consistent scoring (CORRECTED)
#' @param successes Vector of success counts
#' @param totals Vector of total trials
#' @param doses Vector of dose levels
#' @param scoring Scoring method to use
#' @param method Method for phi estimation: "trend_adjusted" or "simple"
estimate_phi_with_scoring <- function(successes, totals, doses, scoring,
                                      method = c("trend_adjusted", "simple")) {

  method <- match.arg(method)

  # Data preparation - EXACTLY as in debug function
  OK <- complete.cases(successes, totals, doses)
  dat <- data.frame(
    successes = successes[OK],
    totals = totals[OK],
    doses = doses[OK]
  ) %>%
    dplyr::group_by(doses) %>%
    dplyr::summarise(
      successes = sum(successes),
      totals = sum(totals),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(doses)

  succ <- dat$successes
  tots <- dat$totals
  dose_vals <- dat$doses

  # Calculate overall proportion
  p_overall <- sum(succ) / sum(tots)
  q_overall <- 1 - p_overall
  n <- length(succ)

  if (method == "simple") {
    # Simple estimate - EXACTLY as in direct calculation
    expected <- tots * p_overall
    residuals <- succ - expected
    phi_estimate <- sum(residuals^2 / (expected * q_overall)) / (n - 1)
    phi_estimate <- max(1, phi_estimate)

    return(phi_estimate)
  }

  # Trend-adjusted method - EXACTLY as in direct calculation
  # Calculate scores
  scores <- switch(scoring,
                   "doses" = dose_vals,
                   "ranks" = seq_along(dose_vals),
                   "log_doses" = {
                     if (any(dose_vals <= 0)) {
                       log(dose_vals + 1)
                     } else {
                       log(dose_vals)
                     }
                   },
                   "equal_spaced" = seq(0, 1, length.out = length(dose_vals))
  )

  # Calculate trend component - EXACTLY as in direct calculation
  mean_score <- sum(tots * scores) / sum(tots)

  # Fit linear trend model
  numerator <- sum(tots * (scores - mean_score) * (succ/tots - p_overall))
  denominator <- sum(tots * (scores - mean_score)^2)

  if (denominator > 0) {
    beta <- numerator / denominator
    trend_proportions <- p_overall + beta * (scores - mean_score)
  } else {
    beta <- 0
    trend_proportions <- rep(p_overall, length(scores))
  }

  # Bounds - EXACTLY as in direct calculation
  trend_proportions <- pmax(0.0001, pmin(0.9999, trend_proportions))

  # Calculate expected counts under trend model
  expected_trend <- tots * trend_proportions

  # Calculate residuals after trend removal
  residuals <- succ - expected_trend

  # Calculate overdispersion parameter - EXACTLY as in direct calculation
  variance_components <- expected_trend * (1 - trend_proportions)
  variance_components <- pmax(variance_components, 0.0001)

  S_adj <- sum(residuals^2 / variance_components)

  # Degrees of freedom adjusted for trend fitting
  df_adj <- length(tots) - 2

  if (df_adj > 0) {
    phi_estimate <- S_adj / df_adj
    phi_estimate <- max(1, phi_estimate)
  } else {
    phi_estimate <- 1
    warning("Insufficient degrees of freedom for trend-adjusted phi estimation")
  }

  return(phi_estimate)
}


#' Compare phi estimates across different scoring methods and estimation methods
#' @param successes Vector of success counts
#' @param totals Vector of total trials
#' @param doses Vector of dose levels
comprehensive_phi_comparison <- function(successes, totals, doses) {

  scoring_methods <- c("doses", "ranks", "log_doses", "equal_spaced")
  estimation_methods <- c("simple", "trend_adjusted")

  # Create a matrix to store results
  results_matrix <- matrix(NA, nrow = length(scoring_methods), ncol = length(estimation_methods))
  rownames(results_matrix) <- scoring_methods
  colnames(results_matrix) <- estimation_methods

  # Calculate all combinations
  for (i in seq_along(scoring_methods)) {
    for (j in seq_along(estimation_methods)) {
      results_matrix[i, j] <- estimate_phi_with_scoring(
        successes, totals, doses,
        scoring_methods[i], estimation_methods[j]
      )
    }
  }

  # Convert to data frame for better display
  results_df <- as.data.frame(results_matrix)
  results_df$Scoring_Method <- rownames(results_matrix)
  results_df <- results_df[, c("Scoring_Method", "simple", "trend_adjusted")]

  # Add interpretation columns
  results_df$Difference <- results_df$trend_adjusted - results_df$simple
  results_df$Interpretation <- ifelse(
    results_df$trend_adjusted > 1.5, "Strong overdispersion (trend-adj)",
    ifelse(results_df$simple > 1.5, "Strong overdispersion (simple)",
           ifelse(pmax(results_df$simple, results_df$trend_adjusted) > 1.2,
                  "Moderate overdispersion", "No overdispersion"))
  )

  cat("Comprehensive Phi Comparison Across Methods\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  print(results_df, row.names = FALSE, digits = 3)

  cat("\nKey Findings:\n")
  cat("- Simple method assumes no dose-response trend\n")
  cat("- Trend-adjusted method removes linear dose-response before testing overdispersion\n")
  cat("- Large differences suggest trend-related vs. random overdispersion\n")

  return(results_df)
}


#' Compare phi estimation methods (FIXED)
#' @param successes Vector of success counts
#' @param totals Vector of total trials
#' @param doses Vector of dose levels
#' @param scoring Scoring method
compare_phi_methods <- function(successes, totals, doses, scoring = "doses") {

  # Make sure we're using the correct function calls
  cat("Calculating phi estimates...\n")

  # Calculate both phi estimates using the FIXED function
  phi_simple <- estimate_phi_with_scoring(successes, totals, doses, scoring, "simple")
  phi_trend <- estimate_phi_with_scoring(successes, totals, doses, scoring, "trend_adjusted")

  cat("Simple method phi:", phi_simple, "\n")
  cat("Trend-adjusted method phi:", phi_trend, "\n")

  # Create comparison table
  comparison_df <- data.frame(
    Method = c("Simple (no trend adjustment)", "Trend-adjusted"),
    Phi_Estimate = c(round(phi_simple, 3), round(phi_trend, 3)),
    Description = c(
      "Based on residuals from overall mean",
      "Based on residuals after removing linear trend"
    ),
    Use_When = c(
      "No clear dose-response trend expected",
      "Clear dose-response trend present"
    )
  )

  cat("\nPhi Estimation Method Comparison (", scoring, " scoring):\n", sep = "")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  print(comparison_df, row.names = FALSE)

  cat("\nRecommendation:\n")
  if (abs(phi_simple - phi_trend) < 0.1) {
    cat("Both methods give similar results. Either can be used.\n")
  } else if (phi_trend > phi_simple) {
    cat("Trend-adjusted method shows higher overdispersion.\n")
    cat("Consider using trend-adjusted if dose-response relationship is expected.\n")
  } else {
    cat("Simple method shows higher overdispersion.\n")
    cat("May indicate overdispersion not related to dose-response trend.\n")
  }

  return(list(phi_simple = phi_simple, phi_trend = phi_trend, comparison = comparison_df))
}


#' Perform complete overdispersion and trend analysis (CORRECTED)
#' @param successes Vector of success counts
#' @param totals Vector of total trials
#' @param doses Vector of dose levels
#' @param scoring Scoring method to use consistently
#' @param alternative Direction of trend test
#' @param phi_method Method for phi estimation when using Rao-Scott
#' @return List with Tarone and step-down results
complete_trend_analysis <- function(successes, totals, doses,
                                    scoring = c("doses", "ranks", "log_doses", "equal_spaced"),
                                    alternative = c("two.sided", "greater", "less"),
                                    phi_method = c("trend_adjusted", "simple")) {

  scoring <- match.arg(scoring)
  alternative <- match.arg(alternative)
  phi_method <- match.arg(phi_method)

  cat("Performing complete trend analysis\n")
  cat("Scoring method:", scoring, "\n")
  cat("Phi estimation method:", phi_method, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Step 1: Tarone overdispersion test
  cat("Step 1: Testing for overdispersion...\n")
  tarone_results <- Tarone.trend.test(
    successes = successes,
    totals = totals,
    doses = doses,
    scoring = scoring,
    include_groupwise_tarone = TRUE
  )

  cat("Tarone test phi estimate:", round(tarone_results$phi_estimate, 3), "\n")

  # Step 2: Calculate phi using specified method for comparison
  phi_simple <- estimate_phi_with_scoring(successes, totals, doses, scoring, "simple")
  phi_trend <- estimate_phi_with_scoring(successes, totals, doses, scoring, "trend_adjusted")

  cat("Simple phi estimate:", round(phi_simple, 3), "\n")
  cat("Trend-adjusted phi estimate:", round(phi_trend, 3), "\n")

  # Choose phi based on method
  phi_to_use <- switch(phi_method,
                       "simple" = phi_simple,
                       "trend_adjusted" = phi_trend
  )

  cat("Using phi =", round(phi_to_use, 3), "for Rao-Scott correction\n\n")

  # Step 3: Step-down Cochran-Armitage test
  cat("Step 2: Step-down Cochran-Armitage trend test...\n")

  # Standard analysis
  stepdown_standard <- stepDownTrendTestBinom(
    successes = successes,
    totals = totals,
    doses = doses,
    scoring = scoring,
    alternative = alternative,
    rao_scott = FALSE
  )

  # Rao-Scott corrected analysis (if overdispersion detected)
  stepdown_corrected <- NULL
  if (phi_to_use > 1.2) {
    cat("Overdispersion detected (φ =", round(phi_to_use, 3),
        "), applying Rao-Scott correction...\n")
    stepdown_corrected <- stepDownTrendTestBinom(
      successes = successes,
      totals = totals,
      doses = doses,
      scoring = scoring,
      alternative = alternative,
      rao_scott = TRUE,
      phi = phi_to_use,
      phi_method = phi_method
    )
  }

  # Compile results
  results <- list(
    scoring_method = scoring,
    phi_method = phi_method,
    phi_estimates = list(
      simple = phi_simple,
      trend_adjusted = phi_trend,
      tarone = tarone_results$phi_estimate,
      used = phi_to_use
    ),
    tarone_analysis = tarone_results,
    stepdown_standard = stepdown_standard,
    stepdown_corrected = stepdown_corrected
  )

  class(results) <- "completeTrendAnalysis"
  return(results)
}


#' Print method for complete trend analysis (UPDATED)
print.completeTrendAnalysis <- function(x, ...) {
  cat("Complete Trend Analysis Results\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Analysis Settings:\n")
  cat("- Scoring Method:", x$scoring_method, "\n")
  cat("- Phi Estimation Method:", x$phi_method, "\n\n")

  # Phi estimates comparison
  cat("Overdispersion Parameter (φ) Estimates:\n")
  phi_df <- data.frame(
    Method = c("Simple (no trend)", "Trend-adjusted", "Tarone test", "Used for correction"),
    Phi_Value = c(
      round(x$phi_estimates$simple, 3),
      round(x$phi_estimates$trend_adjusted, 3),
      round(x$phi_estimates$tarone, 3),
      round(x$phi_estimates$used, 3)
    ),
    Interpretation = c(
      ifelse(x$phi_estimates$simple > 1.5, "Strong overdispersion",
             ifelse(x$phi_estimates$simple > 1.2, "Moderate overdispersion", "No overdispersion")),
      ifelse(x$phi_estimates$trend_adjusted > 1.5, "Strong overdispersion",
             ifelse(x$phi_estimates$trend_adjusted > 1.2, "Moderate overdispersion", "No overdispersion")),
      ifelse(x$phi_estimates$tarone > 1.5, "Strong overdispersion",
             ifelse(x$phi_estimates$tarone > 1.2, "Moderate overdispersion", "No overdispersion")),
      ifelse(x$phi_estimates$used > 1.5, "Strong correction needed",
             ifelse(x$phi_estimates$used > 1.2, "Correction recommended", "No correction needed"))
    )
  )
  print(phi_df, row.names = FALSE)
  cat("\n")

  # NOEC/LOEC results table
  cat("NOEC/LOEC Determination:\n")
  results_df <- data.frame(
    Analysis = c("Standard Cochran-Armitage",
                 if (!is.null(x$stepdown_corrected)) "Rao-Scott Corrected" else NULL),
    NOEC = c(x$stepdown_standard$noec %||% "None",
             if (!is.null(x$stepdown_corrected)) x$stepdown_corrected$noec %||% "None" else NULL),
    LOEC = c(x$stepdown_standard$loec %||% "None",
             if (!is.null(x$stepdown_corrected)) x$stepdown_corrected$loec %||% "None" else NULL),
    Phi_Used = c("1.0 (no correction)",
                 if (!is.null(x$stepdown_corrected)) sprintf("%.3f", x$stepdown_corrected$phi) else NULL)
  )
  print(results_df, row.names = FALSE)
  cat("\n")

  # Recommendation
  cat("Recommendation:\n")
  if (x$phi_estimates$used > 1.5) {
    cat("Strong overdispersion detected. Use Rao-Scott corrected results.\n")
    cat("Final NOEC:", x$stepdown_corrected$noec %||% "None", "\n")
    cat("Final LOEC:", x$stepdown_corrected$loec %||% "None", "\n")
  } else if (x$phi_estimates$used > 1.2) {
    cat("Moderate overdispersion detected. Consider Rao-Scott correction.\n")
    cat("Compare standard and corrected results.\n")
  } else {
    cat("No significant overdispersion. Standard analysis is appropriate.\n")
    cat("Final NOEC:", x$stepdown_standard$noec %||% "None", "\n")
    cat("Final LOEC:", x$stepdown_standard$loec %||% "None", "\n")
  }

  invisible(x)
}

