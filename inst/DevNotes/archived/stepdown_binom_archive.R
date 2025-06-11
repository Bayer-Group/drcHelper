#' Cochran-Armitage Trend Test for Binomial Data
#'
#' Performs a Cochran-Armitage test for trend in binomial proportions across
#' ordered groups, with an optional Rao-Scott correction for overdispersion.
#'
#' @param successes Numeric vector of successes (e.g., number of survivors).
#' @param totals Numeric vector of total observations (e.g., total organisms).
#' @param doses Numeric vector of dose levels or scores corresponding to each observation.
#' @param alternative Character string specifying the alternative hypothesis.
#'        Must be one of "two.sided" (default), "greater", or "less".
#' @param rao_scott Logical indicating whether to apply Rao-Scott correction for
#'        overdispersion (default: FALSE).
#' @param phi Overdispersion parameter for Rao-Scott correction. If NULL (default),
#'        it will be estimated from the data.
#'
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
                                     alternative = c("two.sided", "greater", "less"),
                                     rao_scott = FALSE, phi = NULL) {

  # Match arguments
  alternative <- match.arg(alternative)

  # Check inputs
  if (length(successes) != length(totals) || length(successes) != length(doses)) {
    stop("'successes', 'totals', and 'doses' must have the same length")
  }

  if (any(successes > totals)) {
    stop("'successes' cannot be greater than 'totals'")
  }

  # Remove incomplete cases
  OK <- complete.cases(successes, totals, doses)
  successes <- successes[OK]
  totals <- totals[OK]
  doses <- doses[OK]
  ## Actually we need to use only the summarized datasets grouped by doses.

  dat <- data.frame(successes= successes,totals=totals,doses=doses) %>%
    dplyr::group_by(doses) %>% dplyr::summarise(successes = sum(successes),
                                                totals = sum(totals)) %>% dplyr::ungroup()
  successes <- dat$successes
  totals <- dat$totals
  doses <- dat$doses

  n <- length(successes)
  if (n < 2) stop("At least two groups required")

  # Calculate proportions
  p <- successes / totals

  # Calculate scores (using doses as scores)
  scores <- doses
  ## scores <- seq_along(doses) Note that prop.trend.test function uses this as default score if missing score input.

  # Calculate mean score
  mean_score <- sum(totals * scores) / sum(totals)

  # Calculate numerator: sum(tot_i * (score_i - mean_score) * p_i)
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
  rs_phi <- 1  # Default (no correction)
  if (rao_scott) {
    # Estimate phi if not provided
    if (is.null(phi)) {
      # Simple estimate of overdispersion based on residuals
      expected <- totals * p_overall
      residuals <- successes - expected
      rs_phi <- sum(residuals^2 / (expected * q_overall)) / (n - 1)
      rs_phi <- max(1, rs_phi)  # Ensure phi >= 1
    } else {
      rs_phi <- phi
    }

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
    "Rao-Scott corrected Cochran-Armitage test for trend"
  } else {
    "Cochran-Armitage test for trend"
  }

  data_name <- paste("proportions", paste(round(p, 3), collapse = ", "),
                     "at doses", paste(doses, collapse = ", "))

  result <- list(
    statistic = z_stat,
    p.value = p_value,
    method = method_name,
    alternative = alternative,
    data.name = data_name
  )

  if (rao_scott) {
    result$phi <- rs_phi
  }

  class(result) <- c("htest", "cochranArmitage")
  return(result)
}

#' Step-Down Trend Test for Binomial Data
#'
#' Performs a step-down trend test procedure on binomial data using either the
#' Cochran-Armitage test or Rao-Scott corrected Cochran-Armitage test.
#'
#' @param successes Numeric vector of successes (e.g., number of survivors).
#' @param totals Numeric vector of total observations (e.g., total organisms).
#' @param doses Numeric vector of dose levels corresponding to each observation.
#' @param alternative Character string specifying the alternative hypothesis.
#'        Must be one of "two.sided" (default), "greater", or "less".
#' @param rao_scott Logical indicating whether to apply Rao-Scott correction for
#'        overdispersion (default: FALSE).
#' @param phi Overdispersion parameter for Rao-Scott correction. If NULL (default),
#'        it will be estimated from the data.
#'
#' @return A list with class "stepDownTrendBinom" containing:
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
                                   alternative = c("two.sided", "greater", "less"),
                                   rao_scott = FALSE, phi = NULL) {

  # Match arguments
  alternative <- match.arg(alternative)

  # Check inputs
  if (length(successes) != length(totals) || length(successes) != length(doses)) {
    stop("'successes', 'totals', and 'doses' must have the same length")
  }

  if (any(successes > totals)) {
    stop("'successes' cannot be greater than 'totals'")
  }

  # Remove incomplete cases
  OK <- complete.cases(successes, totals, doses)
  successes <- successes[OK]
  totals <- totals[OK]
  doses <- doses[OK]

  # Order data by doses
  ord <- order(doses)
  successes <- successes[ord]
  totals <- totals[ord]
  doses <- doses[ord]

  # Number of dose groups
  dose_levels <- unique(doses)
  k <- length(dose_levels) ## need to find the number of dose level not the ...
  if (k < 2) {
    stop("at least two dose groups are required")
  }

  # Data name for output
  DNAME <- paste("successes out of totals at doses", paste(doses, collapse = ", "))
  ##browser()
  # Perform step-down procedure
  results <- list()
  for (i in 1:(k-1)) {
    # Select groups from 1 to k-i+1 (removing highest doses first)
    idx_levels <- 1:(k-i+1)
    idx <- which(doses %in% dose_levels[idx_levels])
    # Run test on selected groups
    test_result <- cochranArmitageTrendTest(
      successes = successes[idx],
      totals = totals[idx],
      doses = doses[idx],
      alternative = alternative,
      rao_scott = rao_scott,
      phi = phi
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
  # Find the first non-significant result in the step-down process
  sig_idx <- which(pvals > 0.05)[1]

  if (is.na(sig_idx)) {
    # All tests significant
    noec <- NA
    loec <- dose_levels[1]  # Lowest dose
  } else if (sig_idx == 1) {
    # No significant trend at all
    noec <- doses[k]  # Highest dose
    loec <- NA
  } else {
    # Some significant results
    noec <- dose_levels[k - sig_idx + 1]
    loec <- dose_levels[k - sig_idx + 2]
  }

  # Create output object
  METHOD <- paste0("Step-down ", results[[1]]$method)

  ans <- list(
    method = METHOD,
    data.name = DNAME,
    p.value = PVAL,
    statistic = STAT,
    alternative = alternative,
    doses = doses,
    noec = noec,
    loec = loec
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
