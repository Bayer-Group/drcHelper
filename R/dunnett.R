#' Conduct Dunnett Test with Various Model Specifications
#'
#' This function performs Dunnett's test for comparing multiple treatment levels to a control
#' using various model specifications, including options for random effects and variance structures.
#'
#' @param data A data frame containing the dose-response data
#' Conduct Dunnett Test with Various Model Specifications
#'
#' This function performs Dunnett's test for comparing multiple treatment levels to a control
#' using various model specifications, including options for random effects and variance structures.
#'
#' @param data A data frame containing the dose-response data
#' @param response_var Name of the response variable column
#' @param dose_var Name of the dose/treatment variable column
#' @param block_var Name of the blocking/tank variable column (optional)
#' @param control_level The level of dose_var to use as control (default is minimum dose)
#' @param include_random_effect Logical, whether to include random effects for blocks/tanks
#' @param variance_structure Character, specifying the variance structure:
#'   "homoscedastic" (default) or "heteroscedastic"
#' @param alpha Significance level for determining NOEC (default = 0.05)
#' @param conf_level Confidence level for intervals (default = 0.95)
#' @param return_model Logical, whether to return the fitted model object (default = FALSE)
#' @param alternative a character string specifying the alternative hypothesis, must be
#' one of '"two.sided"' (default), '"greater"' or '"less"'.
#'
#' @return A list containing the Dunnett test results, NOEC value, and optionally the model object
#' @export
#'
#' @importFrom multcomp glht mcp
#' @importFrom lme4 lmer
#' @importFrom nlme gls lme varIdent
#' @importFrom stats as.formula
#' @concept NOEC
dunnett_test <- function(data,
                         response_var = "Response",
                         dose_var = "Dose",
                         block_var = "Tank",
                         control_level = NULL,
                         include_random_effect = TRUE,
                         variance_structure = c("homoscedastic", "heteroscedastic"),
                         alpha = 0.05,
                         conf_level = 0.95,
                         return_model = FALSE,
                         alternative = c("two.sided", "greater", "less")) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

  if (!response_var %in% names(data)) {
    stop(paste("Response variable", response_var, "not found in data"))
  }

  if (!dose_var %in% names(data)) {
    stop(paste("Dose/treatment variable", dose_var, "not found in data"))
  }

  # Ensure dose variable is a factor
  if (!is.factor(data[[dose_var]])) {
    data[[dose_var]] <- factor(data[[dose_var]])
  }

  # Set control level if not specified
  if (is.null(control_level)) {
    # Use the minimum dose level as control
    control_level <- levels(data[[dose_var]])[1]
  } else {
    # Ensure control_level is in the levels
    if (!as.character(control_level) %in% levels(data[[dose_var]])) {
      stop("Control level not found in dose variable levels")
    }
  }

  # Match variance structure argument
  variance_structure <- match.arg(variance_structure)
  alternative <- match.arg(alternative)
  # Check if block variable exists when random effects are requested
  if (include_random_effect && !block_var %in% names(data)) {
    stop(paste("Block/tank variable", block_var, "not found in data"))
  }

  # Create formula strings
  fixed_formula_str <- paste(response_var, "~", dose_var)
  fixed_formula <- as.formula(fixed_formula_str)

  # Fit the appropriate model based on specifications
  if (include_random_effect) {
    if (variance_structure == "homoscedastic") {
      # Mixed model with homoscedastic errors
      message("Fitting mixed model with homoscedastic errors")
      model <- lme4::lmer(
        as.formula(paste(fixed_formula_str, "+ (1|", block_var, ")")),
        data = data
      )
    } else {
      # Mixed model with heteroscedastic errors by dose level
      message("Fitting mixed model with heteroscedastic errors")
      model <- nlme::lme(
        fixed = fixed_formula,
        random = as.formula(paste("~ 1 |", block_var)),
        weights = nlme::varIdent(form = as.formula(paste("~ 1 |", dose_var))),
        data = data
      )
    }
  } else {
    if (variance_structure == "homoscedastic") {
      # Linear model with homoscedastic errors
      message("Fitting linear model with homoscedastic errors")
      model <- stats::lm(fixed_formula, data = data)
    } else {
      # GLS model with heteroscedastic errors by dose level
      message("Fitting GLS model with heteroscedastic errors")
      model <- nlme::gls(
        fixed_formula,
        weights = nlme::varIdent(form = as.formula(paste("~ 1 |", dose_var))),
        data = data
      )
    }
  }

  # Create contrast for Dunnett test
  # This is the corrected part that properly handles variable names
  linfct <- NULL

  if (inherits(model, "lmerMod") || inherits(model, "lm") || inherits(model, "lme")) {
    # For lmer,lme and lm models
    dunnett_args <- list(model)
    mc_formula <- paste(dose_var, "= 'Dunnett'")
    mc_call <- call("mcp")
    mc_call[[dose_var]] <- "Dunnett"

    # Set control level if not the first level
    if (control_level != levels(data[[dose_var]])[1]) {
      mc_call$base <- which(levels(data[[dose_var]]) == as.character(control_level))
    }

    dunnett_args$linfct <- mc_call
    dunnett_args$alternative <- alternative
    dunnett_result <- do.call(multcomp::glht, dunnett_args)

  } else if (inherits(model, "gls")) {
    # For nlme models (lme, gls)
    # Create a contrast matrix manually
    ## browser()
    n_levels <- nlevels(data[[dose_var]])
    control_idx <- which(levels(data[[dose_var]]) == as.character(control_level))

    # Create Dunnett contrast matrix
    K <- matrix(0, n_levels - 1, n_levels)
    row_idx <- 1
    for (i in 1:n_levels) {
      if (i != control_idx) {
        K[row_idx, i] <- 1        # Treatment level
        #K[row_idx, control_idx] <- -1  # Control level
        row_idx <- row_idx + 1
      }
    }

    # Create row names for the contrast matrix
    level_names <- levels(data[[dose_var]])
    row_names <- character(n_levels - 1)
    row_idx <- 1
    for (i in 1:n_levels) {
      if (i != control_idx) {
        row_names[row_idx] <- paste(level_names[i], "-", level_names[control_idx])
        row_idx <- row_idx + 1
      }
    }
    rownames(K) <- row_names

    # Create the contrast
    linfct <- multcomp::glht(model, linfct = K, alternative = alternative)
    dunnett_result <- linfct
  }

  # Get test results
  dunnett_summary <- summary(dunnett_result, test = multcomp::adjusted("single-step"))
  dunnett_confint <- confint(dunnett_result, level = conf_level)

  # Extract p-values and format comparison results
  p_values <- dunnett_summary$test$pvalues
  ##browser()
  comparisons <- rownames(as.data.frame(dunnett_result$linfct))

  # Create a data frame with results
  results_df <- data.frame(
    comparison = comparisons,
    estimate = dunnett_summary$test$coefficients,
    std.error = dunnett_summary$test$sigma,
    statistic = dunnett_summary$test$tstat,
    p.value = p_values,
    conf.low = dunnett_confint$confint[, "lwr"],
    conf.high = dunnett_confint$confint[, "upr"],
    significant = p_values < alpha
  )

  # Determine NOEC (No Observed Effect Concentration)
  # Extract dose levels from comparison strings and convert to numeric
  dose_levels <- sapply(strsplit(comparisons, " - "), function(x) x[1])

  # Convert to numeric if possible
  numeric_doses <- suppressWarnings(as.numeric(dose_levels))
  if (all(!is.na(numeric_doses))) {
    dose_levels <- numeric_doses
  }

  # Find the highest dose with non-significant effect
  significant_effects <- p_values < alpha
  if (all(significant_effects)) {
    noec <- min(dose_levels) # All doses show effects, NOEC is below lowest dose
    noec_message <- "All tested doses show significant effects. NOEC is below the lowest tested dose."
  } else if (!any(significant_effects)) {
    noec <- max(dose_levels) # No doses show effects, NOEC is at or above highest dose
    noec_message <- "No significant effects detected at any dose. NOEC is at or above the highest tested dose."
  } else {
    # Find the highest non-significant dose
    non_sig_doses <- dose_levels[!significant_effects]
    sig_doses <- dose_levels[significant_effects]

    # Ensure we're working with proper numeric values for comparison
    if (is.numeric(non_sig_doses) && is.numeric(sig_doses)) {
      noec <- max(non_sig_doses[non_sig_doses < max(sig_doses)])
    } else {
      # If doses aren't numeric, just return the highest non-significant level
      noec <- non_sig_doses[length(non_sig_doses)]
    }
    noec_message <- paste("NOEC determined as", noec)
  }

  # Prepare return object
  result <- list(
    dunnett_test = dunnett_summary,
    results_table = results_df,
    noec = noec,
    noec_message = noec_message,
    model_type = paste0(
      ifelse(include_random_effect, "Mixed", "Fixed"),
      " model with ",
      variance_structure,
      " errors"
    ),
    control_level = control_level,
    alpha = alpha
  )

  if (return_model) {
    result$model <- model
  }

  class(result) <- "dunnett_test_result"

  return(result)
}

#' Print method for dunnett_test_result objects
#'
#' @param x A dunnett_test_result object
#' @param ... Additional arguments passed to print methods
#'
#' @export
#' @concept NOEC
print.dunnett_test_result <- function(x, ...) {
  cat("Dunnett Test Results\n")
  cat("-------------------\n")
  cat("Model type:", x$model_type, "\n")
  cat("Control level:", x$control_level, "\n")
  cat("Alpha level:", x$alpha, "\n\n")

  cat("Results Table:\n")
  print(x$results_table, row.names = FALSE)

  cat("\nNOEC Determination:\n")
  cat(x$noec_message, "\n")
}

#' Plot method for dunnett_test_result objects
#'
#' @param x A dunnett_test_result object
#' @param ... Additional arguments passed to plot methods
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar theme_minimal labs geom_hline
#' @export
#' @concept NOEC
plot.dunnett_test_result <- function(x, ...) {
  # Extract data for plotting
  plot_data <- x$results_table
  plot_data$comparison <- factor(plot_data$comparison,levels=plot_data$comparison)
  # Create the plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = comparison, y = estimate, color = significant)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Dunnett Test Results:", x$model_type),
      subtitle = paste("NOEC =", x$noec),
      x = "Comparison",
      y = "Difference from Control",
      color = "Significant"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)
}
