#' Perform Many-to-One Welch's t-Tests
#'
#' Compares each level of a factor to a specified control level using Welch's
#' t-test, which does not assume equal variances. It calculates various statistics,
#' including the Minimum Detectable Difference as a percentage of the control mean.
#'
#' @param data A data frame containing the data.
#' @param factor_col String name of the column containing the grouping factor (e.g., "dose").
#' @param response_col String name of the column containing the continuous response variable.
#' @param control_level The level of the factor to be used as the control group.
#'        If NULL (default), the first factor level is used.
#' @param p.adjust.method Method for adjusting p-values for multiple comparisons.
#'        See `?p.adjust` for options (default: "holm").
#' @param alternative A string specifying the alternative hypothesis. Must be one of
#'        "two.sided" (default), "greater", or "less".
#' @param alpha The significance level used to calculate the critical value and MDD%.
#'        Default is 0.05.
#'
#' @return A data frame with results for each comparison, including estimates,
#'         t-statistic, standard error, critical value, MDD%, raw p-value,
#'         and adjusted p-value.
#' @seealso [drcHelper::compare_to_control_fisher()]
#' @export
#' @importFrom stats aggregate p.adjust qt t.test
#' @concept NOEC
#'
#' @examples
#' # Generate example data
#' set.seed(42)
#' my_data <- data.frame(
#'   treatment = rep(c("Control", "Dose1", "Dose2", "Dose3"), each = 5),
#'   response = c(rnorm(5, 10, 1.5), rnorm(5, 9, 2),
#'                rnorm(5, 11, 1.8), rnorm(5, 13, 2.2))
#' )
#'
#' # Run Welch's t-test comparing each dose to "Control"
#' welch_results <- compare_to_control_welch(
#'   data = my_data,
#'   factor_col = "treatment",
#'   response_col = "response",
#'   control_level = "Control"
#' )
#'
#' print(welch_results)

compare_to_control_welch <- function(data, factor_col, response_col,
                                     control_level = NULL,
                                     p.adjust.method = "holm",
                                     alternative = "two.sided",
                                     alpha = 0.05) {

  # --- 1. Validate Inputs ---
  if (!all(c(factor_col, response_col) %in% colnames(data))) {
    stop("One or more specified columns not found in the data frame.")
  }
  if (!is.numeric(data[[response_col]])) {
    stop(paste("Response column '", response_col, "' must be numeric."))
  }

  factor_data <- data[[factor_col]]

  if (is.null(control_level)) {
    control_level <- if(is.factor(factor_data)) levels(factor_data)[1] else sort(unique(factor_data))[1]
    message(paste("`control_level` not specified. Using '", control_level, "' as the control."))
  }

  if (!control_level %in% factor_data) {
    stop(paste("Control level '", control_level, "' not found in the factor column."))
  }

  # --- 2. Pre-calculate Group Statistics ---
  # *** THIS IS THE CORRECTED LINE ***
  # The formula object is now the first argument, and it is not named.
  group_stats <- aggregate(
    as.formula(paste(response_col, "~", factor_col)),
    data = data,
    FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
  )

  group_summary <- do.call(data.frame, group_stats)
  colnames(group_summary) <- c(factor_col, "mean", "sd", "n")

  # --- 3. Prepare for Iteration ---
  control_data <- data[[response_col]][factor_data == control_level]
  control_stats <- group_summary[group_summary[[factor_col]] == control_level, ]

  if(control_stats$mean == 0){
    warning("Control group mean is zero. MDD% will be calculated as Inf or NaN.")
  }

  test_levels <- unique(as.character(factor_data))
  test_levels <- test_levels[test_levels != control_level]

  results_list <- list()

  # --- 4. Loop Through Each Test Group and Perform Test ---
  for (level in test_levels) {
    test_data <- data[[response_col]][factor_data == level]
    test_stats <- group_summary[group_summary[[factor_col]] == level, ]

    test_result <- t.test(
      x = test_data,
      y = control_data,
      alternative = alternative,
      var.equal = FALSE
    )

    se_diff <- sqrt(test_stats$sd^2 / test_stats$n + control_stats$sd^2 / control_stats$n)
    df <- test_result$parameter
    t_critical <- if (alternative == "two.sided") {
      qt(1 - alpha / 2, df)
    } else {
      qt(1 - alpha, df)
    }

    mdd_absolute <- t_critical * se_diff
    mdd_percent <- (mdd_absolute / abs(control_stats$mean)) * 100

    results_list[[level]] <- data.frame(
      p_value = test_result$p.value,
      estimate_diff = test_result$estimate[1] - test_result$estimate[2],
      statistic = test_result$statistic,
      critical_value = t_critical,
      se_diff = se_diff,
      `MDD%` = mdd_percent,
      check.names = FALSE
    )
  }

  # --- 5. Combine, Adjust p-values, and Format Output ---
  if (length(results_list) == 0) {
    message("No test groups to compare against the control.")
    return(data.frame())
  }

  final_results <- do.call(rbind, results_list)
  final_results[[factor_col]] <- rownames(final_results)
  rownames(final_results) <- NULL

  final_results$p_adjusted <- p.adjust(final_results$p_value, method = p.adjust.method)

  final_results <- final_results[, c(
    factor_col, "estimate_diff", "statistic", "critical_value", "se_diff",
    "MDD%", "p_value", "p_adjusted"
  )]

  numeric_cols <- sapply(final_results, is.numeric)
  final_results[numeric_cols] <- lapply(final_results[numeric_cols], round, 4)

  return(final_results)
}
