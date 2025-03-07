## Function to calculate NOECs using multiple methods and broomed together
## Unit Testing in # FILE: tests/testthat/test_quantal_categorical.R



#' Calculate NOEC Using Many-to-one Pairwise Tests
#'
#' This function calculates the No Observed Effect Concentration (NOEC) from dose response data
#' using pairwise comparison tests from the rstatix package.
#'
#' @param data A data frame containing the dose response data
#' @param response The name of the response variable (unquoted)
#' @param dose The name of the dose variable (unquoted)
#' @param control The level of the dose variable to be used as control
#' @param test The statistical test to use: "t.test" or "wilcox.test"
#' @param p_adjust_method Method for p-value adjustment for multiple comparisons
#' @param alternative Direction of the alternative hypothesis: "two.sided", "greater", or "less"
#' @param alpha Significance level (default: 0.05)
#'
#' @return A list containing the NOEC value and the full test results
#' @export
#'
#' @importFrom dplyr mutate filter arrange %>%
#' @importFrom rlang enquo quo_name .data
#' @importFrom rstatix t_test wilcox_test
#' @importFrom stats as.formula setNames
calculate_noec_rstatix <- function(data, response, dose, control = "0",
                                   test = c("t.test", "wilcox.test"),
                                   p_adjust_method = "holm",
                                   alternative = "two.sided",
                                   alpha = 0.05) {

  # Match test argument
  test <- match.arg(test)

  # Capture variable names using rlang
  response_var <- rlang::enquo(response)
  dose_var <- rlang::enquo(dose)
  response_name <- rlang::quo_name(response_var)
  dose_name <- rlang::quo_name(dose_var)

  # Ensure dose is a factor
  if (!is.factor(data[[dose_name]])) {
    data[[dose_name]] <- as.factor(data[[dose_name]])
  }

  # Create formula
  formula_obj <- stats::as.formula(paste(response_name, "~", dose_name))

  # Run the appropriate test
  if (test == "t.test") {
    test_results <- rstatix::t_test(
      data = data,
      formula = formula_obj,
      ref.group = control,
      p.adjust.method = p_adjust_method,
      alternative = alternative
    )
  } else if (test == "wilcox.test") {
    test_results <- rstatix::wilcox_test(
      data = data,
      formula = formula_obj,
      ref.group = control,
      p.adjust.method = p_adjust_method,
      alternative = alternative
    )
  }

  # Extract dose levels and convert to numeric for sorting
  dose_levels <- levels(data[[dose_name]])
  numeric_doses <- suppressWarnings(as.numeric(dose_levels))

  # If conversion to numeric was successful, reorder the test results
  if (!any(is.na(numeric_doses))) {
    # Create a mapping from dose level to numeric value
    dose_mapping <- stats::setNames(numeric_doses, dose_levels)

    # Add numeric dose values to test results
    test_results <- test_results %>%
      dplyr::mutate(
        group1_numeric = dose_mapping[.data$group1],
        group2_numeric = dose_mapping[.data$group2]
      )

    # Determine which column contains the non-control doses
    if (all(test_results$group1 == control)) {
      test_results <- test_results %>%
        dplyr::mutate(dose_numeric = .data$group2_numeric)
    } else if (all(test_results$group2 == control)) {
      test_results <- test_results %>%
        dplyr::mutate(dose_numeric = .data$group1_numeric)
    } else {
      # Mixed case - need to handle both possibilities
      test_results <- test_results %>%
        dplyr::mutate(
          dose_numeric = ifelse(.data$group1 == control,
                                .data$group2_numeric,
                                .data$group1_numeric)
        )
    }

    # Sort by numeric dose
    test_results <- test_results %>%
      dplyr::arrange(.data$dose_numeric)
  }

  # Find the NOEC (highest dose with p > alpha)
  significant_results <- test_results %>%
    dplyr::filter(.data$p.adj <= alpha)

  if (nrow(significant_results) == 0) {
    # If no significant differences, NOEC is the highest tested dose
    all_doses <- as.numeric(as.character(unique(data[[dose_name]])))
    all_doses <- all_doses[!is.na(all_doses) & all_doses != as.numeric(control)]

    if (length(all_doses) > 0) {
      noec_value <- max(all_doses, na.rm = TRUE)
      noec_message <- "No significant effects detected at any dose level"
    } else {
      noec_value <- NA
      noec_message <- "No valid dose levels found"
    }
  } else {
    # Get all doses with significant effects
    significant_doses <- c()

    for (i in 1:nrow(significant_results)) {
      row <- significant_results[i, ]
      if (row$group1 == control) {
        significant_doses <- c(significant_doses, as.character(row$group2))
      } else {
        significant_doses <- c(significant_doses, as.character(row$group1))
      }
    }

    # Convert to numeric for comparison
    significant_doses <- suppressWarnings(as.numeric(significant_doses))

    # Get all tested doses
    all_doses <- suppressWarnings(as.numeric(as.character(unique(data[[dose_name]]))))
    all_doses <- all_doses[!is.na(all_doses) & all_doses != as.numeric(control)]
    all_doses <- sort(all_doses)

    # Find the highest non-significant dose
    non_significant_doses <- all_doses[!all_doses %in% significant_doses]

    if (length(non_significant_doses) == 0) {
      # If all doses show significant effects
      noec_value <- as.numeric(control)
      noec_message <- "All tested doses showed significant effects"
    } else {
      noec_value <- max(non_significant_doses)
      noec_message <- paste("NOEC determined as", noec_value)
    }
  }

  # Return both the NOEC value and the full test results
  return(list(
    noec = noec_value,
    noec_message = noec_message,
    test_results = test_results
  ))
}













