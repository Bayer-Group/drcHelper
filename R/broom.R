## Brooming the different test results together so that they can be compared

#' Standardize Williams Test Results
#'
#' This function creates a standardized output from different Williams test implementations.
#'
#' @param x A formula object specifying the response variable and the factor for which the test is to be performed,
#'          or an object of class 'aov' or 'lm'.
#' @param method Character string specifying which implementation of Williams test to use:
#'        "Williams_PMCMRplus" (default) for PMCMRplus implementation or
#'        "Williams_JG" for the drcHelper custom implementation.
#' @param ... Additional arguments passed to the underlying test functions.
#'
#' @return A data frame with standardized test results containing:
#'   \item{comparison}{The comparison being made}
#'   \item{estimate}{The estimated difference between groups}
#'   \item{p.value}{The p-value for the test}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{method}{The method used for the test}
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
#' # Apply Williams test
#' result <- broom_williams(response ~ dose, data = test_data)
#' print(result)
#'
#' @importFrom dplyr mutate select filter arrange %>%
#' @importFrom tibble tibble
#' @importFrom PMCMRplus williamsTest
#' @export
broom_williams <- function(x, method = c("Williams_PMCMRplus", "Williams_JG"), ...) {
  method <- match.arg(method)

  if (method == "Williams_PMCMRplus") {
    williams_result <- try({
      wt <- PMCMRplus::williamsTest(x, ...)

      # Extract summary information
      wt_summary <- summary(wt)
      comparisons <- names(wt_summary$p.value)

      # Create standardized output
      result <- tibble::tibble(
        comparison = comparisons,
        estimate = wt_summary$estimate,
        p.value = wt_summary$p.value,
        conf.low = wt_summary$confint[, 1],
        conf.high = wt_summary$confint[, 2],
        method = "Williams_PMCMRplus"
      )

      result
    }, silent = TRUE)
  } else if (method == "Williams_JG") {
    williams_result <- try({
      wt <- drcHelper::williamsTest_JG(x, ...)

      # Assuming williamsTest_JG returns a data frame with standardized columns
      # If not, you'll need to transform its output to match the standard format
      wt$method <- "Williams_JG"

      wt
    }, silent = TRUE)
  }

  if (inherits(williams_result, "try-error")) {
    warning("Williams test not conducted, probably due to input being a formular. Error: ",
            attr(williams_result, "condition")$message)
    return(tibble::tibble(
      comparison = character(),
      estimate = numeric(),
      p.value = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      method = character()
    ))
  }

  return(williams_result)
}

#' Standardize Dunnett Test Results
#'
#' This function creates a standardized output from different Dunnett test implementations.
#'
#' @param x A formula object specifying the response variable and the factor for which the test is to be performed,
#'          or an object of class 'aov' or 'lm'.
#' @param method Character string specifying which implementation of Dunnett test to use:
#'        "Dunnett_multcomp" (default) for multcomp implementation,
#'        "Dunnett_DescTools" for DescTools implementation, or
#'        "Dunnett_PMCMRplus" for PMCMRplus implementation.
#' @param ... Additional arguments passed to the underlying test functions.
#'
#' @return A data frame with standardized test results containing:
#'   \item{comparison}{The comparison being made}
#'   \item{estimate}{The estimated difference between groups}
#'   \item{p.value}{The p-value for the test}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{method}{The method used for the test}
#'
#' @importFrom dplyr mutate select filter arrange %>%
#' @importFrom tibble tibble
#' @importFrom multcomp glht mcp
#' @importFrom stats aov confint
#' @importFrom PMCMRplus dunnettTest
#' @importFrom DescTools DunnettTest
#' @export
broom_dunnett <- function(x, method = c("Dunnett_multcomp", "Dunnett_DescTools", "Dunnett_PMCMRplus"), ...) {
  method <- match.arg(method)

  if (method == "Dunnett_PMCMRplus") {
    result <- try({
      dt <- PMCMRplus::dunnettTest(x, ...)

      # Extract summary information
      dt_summary <- summary(dt)
      comparisons <- names(dt_summary$p.value)

      # Create standardized output
      tibble::tibble(
        comparison = comparisons,
        estimate = dt_summary$estimate,
        p.value = dt_summary$p.value,
        conf.low = dt_summary$confint[, 1],
        conf.high = dt_summary$confint[, 2],
        method = "Dunnett_PMCMRplus"
      )
    }, silent = TRUE)
  } else if (method == "Dunnett_DescTools") {
    result <- try({
      dt <- DescTools::DunnettTest(x, ...)

      # Extract the first element if it's a list (DescTools returns a list for multiple controls)
      if (is.list(dt) && !is.data.frame(dt)) {
        dt <- dt[[1]]
      }

      # Convert to tibble with standardized column names
      tibble::tibble(
        comparison = rownames(dt),
        estimate = dt[, "diff"],
        p.value = dt[, "pval"],
        conf.low = dt[, "lwr"],
        conf.high = dt[, "upr"],
        method = "Dunnett_DescTools"
      )
    }, silent = TRUE)
  } else if (method == "Dunnett_multcomp") {
    result <- try({
      # For multcomp, we need to create an aov model first
      if (inherits(x, "formula")) {
        model_data <- list(...)$data
        if (is.null(model_data)) {
          model_data <- environment(x)
        }
        aov_model <- aov(x, data = model_data)
      } else if (inherits(x, c("aov", "lm"))) {
        aov_model <- x
      } else {
        stop("x must be a formula, aov, or lm object for Dunnett_multcomp method")
      }

      # Extract the factor name from the formula
      if (inherits(x, "formula")) {
        terms <- terms(x)
        factor_name <- attr(terms, "term.labels")[1]
      } else {
        factor_name <- names(aov_model$xlevels)[1]
      }

      # Get control level
      control <- list(...)$control
      if (is.null(control)) {
        control <- 1  # Default to first level
      }

      # Create contrast matrix for Dunnett
      contrasts_call <- call("mcp")
      contrasts_call[[2]] <- as.name(factor_name)
      contrasts_call[[3]] <- "Dunnett"
      names(contrasts_call)[3] <- factor_name
      contrasts_call$base <- control


      dunnett_contrasts <- eval(contrasts_call)

      # Run the test
      glht_result <- multcomp::glht(aov_model, linfct = dunnett_contrasts)
      summary_result <- summary(glht_result)
      conf_int <- confint(glht_result)

      # Create standardized output
      tibble::tibble(
        comparison = names(summary_result$test$pvalues),
        estimate = summary_result$test$coefficients,
        p.value = summary_result$test$pvalues,
        conf.low = conf_int$confint[, "lwr"],
        conf.high = conf_int$confint[, "upr"],
        method = "Dunnett_multcomp"
      )
    }, silent = TRUE)
  }

  if (inherits(result, "try-error")) {
    warning("Dunnett test failed. Error: ", attr(result, "condition")$message)
    return(tibble::tibble(
      comparison = character(),
      estimate = numeric(),
      p.value = numeric(),
      conf.low = numeric(),
      conf.high = numeric(),
      method = character()
    ))
  }

  return(result)
}

