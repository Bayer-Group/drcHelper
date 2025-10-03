#' Calculate MDD% for a Williams Test Result
#'
#' @param williams_obj The tibble result from broom_williams.
#' @param data The original dataframe used for the test.
#' @param formula The formula used for the test, e.g., Response ~ Dose.
#' @return A tibble with Dose, MDD, and MDD_pct.
#' @export
compute_mdd_williams <- function(williams_obj, data, formula) {

  if (!inherits(williams_obj, "tbl_df") || !all(c("comparison", "t'-crit") %in% names(williams_obj))) {
    stop("williams_obj must be the result from a broom_williams call.")
  }
  if (nrow(williams_obj) == 0) return(tibble::tibble(Dose=numeric(), MDD_pct=numeric()))

  # 1. Extract variable names
  resp_name <- all.vars(formula)[1]
  dose_name <- all.vars(formula)[2] # Correctly gets "Dose"

  # --- START OF FIX ---
  # Create a local copy of the data to avoid modifying the original
  local_data <- data

  # Ensure the dose column is a factor with control (0) as the first level.
  # This makes the function robust and independent of the input data's column type.
  dose_values <- local_data[[dose_name]]
  factor_levels <- sort(unique(dose_values))
  local_data$dose_factor_col <- factor(dose_values, levels = factor_levels)

  # Define the new, reliable factor column name and update formula for aov()
  factor_col_name <- "dose_factor_col"
  aov_formula <- as.formula(paste(resp_name, "~", factor_col_name))
  # --- END OF FIX ---

  # 2. Extract info from the Williams test object
  Tcrit <- williams_obj[["t'-crit"]]
  doses <- as.numeric(gsub(" - 0.*", "", williams_obj$comparison))

  # 3. Get ANOVA stats (MSE) using the corrected formula and local data
  aov_fit <- stats::aov(aov_formula, data = local_data)
  mse <- summary(aov_fit)[[1]]["Residuals", "Mean Sq"]

  # 4. Get control group stats using the reliable factor column
  control_level <- levels(local_data[[factor_col_name]])[1]
  ctrl_data <- local_data[local_data[[factor_col_name]] == control_level, ]
  mu_c <- mean(ctrl_data[[resp_name]], na.rm = TRUE)
  n_c <- nrow(ctrl_data)

  # 5. Get treatment sample sizes
  n_t <- sapply(doses, function(d) sum(local_data[[dose_name]] == d))

  # 6. Calculate MDD and MDD%
  SE_diff <- sqrt(mse * (1 / n_c + 1 / n_t))
  MDD <- Tcrit * SE_diff
  MDD_pct <- 100 * MDD / abs(mu_c)

  tibble::tibble(Dose = doses, MDD_pct = MDD_pct)
}




#' Calculate MDD% for a Dunnett Test Result
#'
#' @param dunnett_obj The result object from your dunnett_test function.
#' @param data The original dataframe used for the test.
#' @param formula The formula used for the test, e.g., Response ~ Dose.
#' @return A tibble with Dose, MDD, and MDD_pct.
#' Calculate MDD% for a Dunnett Test Result (Robust Version 2)
#'
#' @param dunnett_obj The result object from your dunnett_test function.
#' @param alternative The alternative hypothesis ("less", "greater", "two.sided").
#' @param data The original dataframe used for the test.
#' @param formula The formula used for the test.
#' @return A tibble with Dose and MDD_pct.
#' @export
compute_mdd_dunnett <- function(dunnett_obj, alternative, data, formula) {

  # 1. Validate input
  # --- START OF FIX ---
  # Determine the actual results table from the input object
  results_table <- NULL
  if (is.data.frame(dunnett_obj)) {
    # This handles the direct tibble from broom_dunnett
    results_table <- dunnett_obj
  } else if (is.list(dunnett_obj) && "results_table" %in% names(dunnett_obj)) {
    # This handles the output from the original dunnett_test function
    results_table <- dunnett_obj$results_table
  } else {
    stop("Input 'dunnett_obj' is not a recognized result format.")
  }
  # --- END OF FIX ---
  if (nrow(results_table) == 0) return(tibble::tibble(Dose = numeric(), MDD_pct = numeric()))

  # 2. Extract variable names and ensure dose column is factored
  resp_name <- all.vars(formula)[1]
  dose_name <- all.vars(formula)[2]
  local_data <- data
  local_data$dose_factor_col <- factor(local_data[[dose_name]], levels = sort(unique(local_data[[dose_name]])))
  factor_col_name <- "dose_factor_col"

  # 3. Derive the adjusted critical value (Tcrit) using the explicit 'alternative'
  first_row <- results_table[1, ]
  Tcrit <- NA

  if (alternative == "less") {
    Tcrit <- (first_row$conf.high - first_row$estimate) / first_row$std.error
  } else if (alternative == "greater") {
    Tcrit <- (first_row$estimate - first_row$conf.low) / first_row$std.error
  } else if (alternative == "two.sided") {
    Tcrit <- (first_row$conf.high - first_row$estimate) / first_row$std.error
  }

  if (is.na(Tcrit) || is.infinite(Tcrit) || Tcrit < 0) {
    stop("Could not derive a valid Tcrit from confidence intervals for the given alternative.")
  }

  # 4. Get control mean
  control_level <- levels(local_data[[factor_col_name]])[1]
  mu_c <- mean(local_data[local_data[[factor_col_name]] == control_level, resp_name], na.rm = TRUE)

  # 5. Calculate MDD and MDD%
  SE_diff <- first_row$std.error
  MDD <- Tcrit * SE_diff
  MDD_pct <- 100 * MDD / abs(mu_c)

  # 6. Create the final tibble
  doses <- as.numeric(gsub(" - 0.*", "", results_table$comparison))
  tibble::tibble(Dose = doses, MDD_pct = rep(MDD_pct, length(doses)))
}


#' Generate a Comprehensive Dunnett Test Summary Report
#'
#' This high-level wrapper function performs a Dunnett's test to compare multiple
#' treatment groups against a single control group. It is designed to be flexible,
#' allowing for different underlying test functions and handling both standard
#' fixed-effects (ANOVA) and mixed-effects models. The function combines the
#' statistical test results with a calculated Minimum Detectable Difference (MDD%)
#' into a single, formatted output table.
#'
#' @param formula A model formula, e.g., `Response ~ Dose` for a fixed-effects model,
#'   or `Response ~ Dose + (1|Batch)` for a mixed-effects model.
#' @param data A data frame containing the data specified in the formula.
#' @param dunnett_test_func A function (passed as a character string or as a function object)
#'   that performs the Dunnett test and is expected to return an object of class `glht`.
#'   See `multcomp::glht` for details.
#' @param alternative A character string specifying the alternative hypothesis. Must be one
#'   of "two.sided", "less" (default), or "greater".
#' @param include_random_effect A logical value. Set to `TRUE` if the `formula` includes
#'   a random effect term (e.g., `(1|Batch)`), which requires a mixed-effects model.
#'   Defaults to `FALSE`.
#' @param ... Additional arguments to be passed to the function specified in
#'   `dunnett_test_func`. A common argument is `alpha` to set the significance
#'   level (e.g., `alpha = 0.05`).
#'
#' @return A data frame containing the comprehensive results for each comparison against
#'   the control, with columns for the comparison name, estimate difference, test
#'   statistic, critical value, standard error of the difference, MDD%, and p-value.
#'
#' @export
#' @seealso [multcomp::glht()]
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' set.seed(42)
#' my_data <- data.frame(
#'   Dose = factor(rep(c(0, 5, 20, 50), each = 6)),
#'   Response = c(rnorm(6, 100, 10), rnorm(6, 90, 12),
#'                rnorm(6, 85, 10), rnorm(6, 80, 11))
#' )
#'
#' # Run the summary function for a one-sided test (less than control)
#' summary_results <- report_dunnett_summary(
#'   formula = Response ~ Dose,
#'   data = my_data,
#'   dunnett_test_func = "dunnett_test", # Your custom test function
#'   alternative = "less",
#'   alpha = 0.05
#' )
#'
#' print(summary_results)
#' }
report_dunnett_summary <- function(formula, data, dunnett_test_func, alternative = "less",
                                   include_random_effect = FALSE,...) {

  resp_name <- all.vars(formula)[1]
  dose_name <- all.vars(formula)[2]

  # 1. Preliminary Summary
  prelim_stats <- prelimSummary(data, dose_col = dose_name, response_col = resp_name)

  # 2. Run the Dunnett Test
  if(dunnett_test_func == "dunnett_test") dunnett_results_obj <- dunnett_test(
    data = data,
    response_var = resp_name,
    dose_var = dose_name,
    alternative = alternative, # Pass alternative to the test
    include_random_effect = include_random_effect,
    ...
  )
  if(dunnett_test_func == "broom_dunnett") dunnett_results_obj <- broom_dunnett(formula,
    data = data,
    alternative = alternative, # Pass alternative to the test
    ...
  )

  # 3. MDD% Calculation
  mdd_results <- compute_mdd_dunnett(
    dunnett_obj = dunnett_results_obj,
    alternative = alternative, # Pass the same alternative to the MDD function
    data = data,
    formula = formula
  )
  mdd_results$Dose <- factor(mdd_results$Dose)
  names(mdd_results)[1] <- dose_name
  # --- START OF FIX ---
  # Determine the actual results table from the input object
  results_table <- NULL
  if (is.data.frame(dunnett_results_obj)) {
    # This handles the direct tibble from broom_dunnett
    results_table <- dunnett_results_obj
  } else if (is.list(dunnett_results_obj) && "results_table" %in% names(dunnett_results_obj)) {
    # This handles the output from the original dunnett_test function
    results_table <- dunnett_results_obj$results_table
  } else {
    stop("Input 'dunnett_obj' is not a recognized result format.")
  }
  # --- END OF FIX ---
  # 4. Tidy and Join Results
  test_output <- results_table %>%
    dplyr::mutate(Dose = as.numeric(gsub(" - 0.*", "", comparison))) %>%
    dplyr::select(Dose, statistic, p.value, significant)
  test_output$Dose <- factor(test_output$Dose)
  names(test_output)[1] <- dose_name
  final_report <- prelim_stats %>%
    dplyr::left_join(test_output, by = dose_name) %>%
    dplyr::left_join(mdd_results, by = dose_name) %>%
    dplyr::rename(MDD_Dunnett_Percent = MDD_pct, t_statistic = statistic)

  # 5. Reorder and return
  final_report %>%
    dplyr::select(
      Dose, Mean, SD, CV, `% Inhibition`,
      t_statistic, p.value, significant, MDD_Dunnett_Percent
    )
}
