#' Standardize Step-Down Trend Test Results
#'
#' This function creates a standardized output from step-down trend test implementations.
#'
#' @param x A formula object specifying the response variable and the factor for which the test is to be performed,
#'          or an object of class 'aov' or 'lm'.
#' @param method Character string specifying which implementation to use (currently only "stepDown_PMCMR" is supported).
#' @param test Character string specifying which trend test to use (default: "jonckheereTest").
#' @param ... Additional arguments passed to the underlying test functions.
#'
#' @return A data frame with standardized test results containing:
#'   \item{comparison}{The comparison being made}
#'   \item{p.value}{The p-value for the test}
#'   \item{method}{The method used for the test}
#'   \item{statistic}{The test statistic}
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' test_data <- data.frame(
#'   dose = factor(rep(c(0, 1, 5, 10), each = 5)),
#'   response = c(rnorm(5, 100, 10), rnorm(5, 90, 10),
#'                rnorm(5, 80, 10), rnorm(5, 70, 10))
#' )
#'
#' # Apply step-down trend test
#' result <- broom_stepDown(response ~ dose, data = test_data)
#' print(result)
#'
#' @importFrom dplyr mutate select filter arrange %>%
#' @importFrom tibble tibble
#' @importFrom PMCMRplus stepDownTrendTest
#' @export
broom_stepDown <- function(x, method = c("stepDown_PMCMR"), test = "jonckheereTest", ...) {
  method <- match.arg(method)

  if (method == "stepDown_PMCMR") {
    result <- try({
      res <- PMCMRplus::stepDownTrendTest(x, test = test, ...)

      # Extract results
      p_values <- res$p.value
      comparisons <- names(p_values)

      # Create standardized output
      tibble::tibble(
        comparison = comparisons,
        p.value = p_values,
        method = paste0("stepDown_", test),
        statistic = res$statistic
      )
    }, silent = TRUE)
  }

  if (inherits(result, "try-error")) {
    warning("Step-down trend test failed. Error: ", attr(result, "condition")$message)
    return(tibble::tibble(
      comparison = character(),
      p.value = numeric(),
      method = character(),
      statistic = numeric()
    ))
  }

  return(result)
}

#' Combine Multiple Comparison Test Results
#'
#' This function combines the results of different multiple comparison tests
#' for dose response data analysis into a single data frame for easy comparison.
#'
#' @param data A data frame containing the dose response data
#' @param response The name of the response variable (unquoted)
#' @param dose The name of the dose variable (unquoted)
#' @param group An optional grouping variable (unquoted)
#' @param control The level of the dose variable to be used as control
#' @param alternative Direction of the alternative hypothesis: "two.sided", "greater", or "less"
#' @param conf.level Confidence level for the confidence intervals
#' @param methods Character vector specifying which methods to include. Options are:
#'        "Dunnett_multcomp", "Dunnett_DescTools", "Dunnett_PMCMRplus",
#'        "Williams_PMCMRplus", "Williams_JG", "stepDown_PMCMR"
#'
#' @return A tibble containing the combined results of the specified multiple comparison tests
#' @export
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' test_data <- data.frame(
#'   dose = factor(rep(c(0, 1, 5, 10), each = 5)),
#'   response = c(rnorm(5, 100, 10), rnorm(5, 90, 10),
#'                rnorm(5, 80, 10), rnorm(5, 70, 10))
#' )
#'
#' # Combine multiple comparison test results
#' results <- combine_multiple_comparisons(
#'   data = test_data,
#'   response = response,
#'   dose = dose,
#'   control = "0",
#'   alternative = "less"
#' )
#'
#' # View the results
#' results
#'
#' @importFrom dplyr mutate select filter arrange %>% bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom rlang enquo quo_name
#' @importFrom stats as.formula
combine_multiple_comparisons <- function(data, response, dose, group = NULL,
                                         control = 0, alternative = "two.sided",
                                         conf.level = 0.95,
                                         methods = c("Dunnett_multcomp", "Williams_PMCMRplus", "stepDown_PMCMR")) {

  # Capture variable names using rlang
  response_var <- rlang::enquo(response)
  dose_var <- rlang::enquo(dose)
  response_name <- rlang::quo_name(response_var)
  dose_name <- rlang::quo_name(dose_var)

  # Create formula for tests
  if (!is.null(group)) {
    group_var <- rlang::enquo(group)
    group_name <- rlang::quo_name(group_var)
    formula_str <- paste(response_name, "~", dose_name, "*", group_name)
  } else {
    formula_str <- paste(response_name, "~", dose_name)
  }

  formula_obj <- as.formula(formula_str)

  # Initialize results list
  all_results <- list()

  # Common parameters for all tests
  common_params <- list(
    x = formula_obj,
    data = data,
    alternative = alternative,
    conf.level = conf.level,
    control = control
  )

  # Run selected methods
  for (method in methods) {
    if (method == "Dunnett_multcomp") {
      result <- do.call(broom_dunnett, c(common_params, list(method = "Dunnett_multcomp")))
      all_results[["Dunnett_multcomp"]] <- result
    }
    else if (method == "Dunnett_DescTools") {
      result <- do.call(broom_dunnett, c(common_params, list(method = "Dunnett_DescTools")))
      all_results[["Dunnett_DescTools"]] <- result
    }
    else if (method == "Dunnett_PMCMRplus") {
      result <- do.call(broom_dunnett, c(common_params, list(method = "Dunnett_PMCMRplus")))
      all_results[["Dunnett_PMCMRplus"]] <- result
    }
    else if (method == "Williams_PMCMRplus") {
      result <- do.call(broom_williams, c(common_params, list(method = "Williams_PMCMRplus")))
      all_results[["Williams_PMCMRplus"]] <- result
    }
    else if (method == "Williams_JG") {
      result <- do.call(broom_williams, c(common_params, list(method = "Williams_JG")))
      all_results[["Williams_JG"]] <- result
    }
    else if (method == "stepDown_PMCMR") {
      result <- do.call(broom_stepDown, c(common_params, list(method = "stepDown_PMCMR")))
      all_results[["stepDown_PMCMR"]] <- result
    }
  }

  # Combine all results
  combined_results <- dplyr::bind_rows(all_results)

  # Add significance indicator based on confidence level
  alpha <- 1 - conf.level
  combined_results <- combined_results %>%
    dplyr::mutate(significant = p.value < alpha)

  # Create a wide format table for easy comparison
  comparison_table <- combined_results %>%
    dplyr::select(comparison, method, p.value, significant) %>%
    tidyr::pivot_wider(
      id_cols = comparison,
      names_from = method,
      values_from = c(p.value, significant),
      names_sep = "_"
    )

  # Return both the detailed results and the comparison table
  return(list(
    detailed_results = combined_results,
    comparison_table = comparison_table
  ))
}

