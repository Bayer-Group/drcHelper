#' Determine NOEC/LOEC using a Step-down Trend Test
#'
#' This function acts as a wrapper around PMCMRplus::stepDownTrendTest to
#' directly calculate the No-Observed-Effect Concentration (NOEC) and
#' Lowest-Observed-Effect Concentration (LOEC).
#'
#' It interprets the "step-up" ordered results from the underlying PMCMRplus
#' function by searching backwards from the full test to find the first
#' non-significant result (p >= alpha), as is standard for NOEC determination.
#'
#' @param formula A formula of the form `response ~ group`.
#' @param data A data frame containing the variables. The group variable should be a factor.
#' @param test The trend test to use (passed to PMCMRplus). Defaults to "jonckheereTest".
#' @param alpha The significance level to use for determining the NOEC. Defaults to 0.05.
#' @param ... Other arguments passed to `PMCMRplus::stepDownTrendTest`.
#'
#' @return A list of class `noecTrendTest` containing:
#'   \item{NOEC}{The determined No-Observed-Effect Concentration.}
#'   \item{LOEC}{The determined Lowest-Observed-Effect Concentration.}
#'   \item{alpha}{The significance level used.}
#'   \item{full_results}{The complete, original `trendPMCMR` object for inspection.}
#' @export
noec_from_trend_test <- function(formula, data, test = "jonckheereTest", alpha = 0.05, ...) {

  # --- Step 1: Input validation ---
  group_var_name <- all.vars(formula)[2]
  if (!is.factor(data[[group_var_name]])) {
    warning(paste("Coercing group variable", group_var_name, "to a factor for correct ordering."))
    data[[group_var_name]] <- factor(data[[group_var_name]])
  }

  # --- Step 2: Run the underlying PMCMRplus test ---
  # This gives us all the p-values, albeit in the "step-up" order.
  results_obj <- PMCMRplus::stepDownTrendTest(
    formula = formula,
    data = data,
    test = test,
    ...
  )

  # --- Step 3: Extract p-values and dose levels ---
  p_values <- results_obj$p.value
  dose_levels <- rownames(p_values)
  control_level <- colnames(p_values)[1]
  all_levels <- c(control_level, dose_levels)

  # --- Step 4: Find the NOEC by searching backwards ---
  noec <- NA

  # Iterate from the full test (last row) backwards to the first test (first row)
  for (i in nrow(p_values):1) {
    # Check if this test is NOT significant
    if (p_values[i, 1] >= alpha) {
      # If it's not significant, the highest dose in this test is the NOEC.
      noec <- dose_levels[i]
      break # We found the NOEC, so we can stop searching.
    }
  }

  # --- Step 5: Handle edge cases ---
  loec <- NA

  # Case 1: The loop finished without finding a non-significant test.
  # This means all tests were significant, so NOEC is the control.
  if (is.na(noec)) {
    noec <- control_level
    loec <- dose_levels[1] # The lowest tested dose is the LOEC
  } else {
    # Case 2: A NOEC was found. The LOEC is the next dose level up.
    noec_index <- which(all_levels == noec)
    # Check if the NOEC is the highest tested dose
    if (noec_index == length(all_levels)) {
      loec <- "Not Detected"
    } else {
      loec <- all_levels[noec_index + 1]
    }
  }

  # --- Step 6: Create a clean output object ---
  output <- list(
    NOEC = noec,
    LOEC = loec,
    alpha = alpha,
    full_results = results_obj
  )

  class(output) <- "noecTrendTest"

  return(output)
}

#' Print method for noecTrendTest objects
#' @param x An object of class `noecTrendTest`.
#' @param ... Other arguments (not used).
#' @method print noecTrendTest
print.noecTrendTest <- function(x, ...) {
  cat("--- Step-Down Trend Test for NOEC Determination ---\n")
  cat("Test method:", x$full_results$method, "\n\n")
  cat("NOEC:", x$NOEC, "\n")
  cat("LOEC:", x$LOEC, "\n")
  cat("Alpha level:", x$alpha, "\n\n")
  cat("--- Full PMCMRplus Results ---\n")
  full_results <- x$full_results
  summary(full_results)
  invisible(x)
}











#' A Wrapper for PMCMRplus::stepDownTrendTest for NOEC Determination
#'
#' This function calls PMCMRplus::stepDownTrendTest but reorders the results
#' to follow a true "step-down" procedure, making it intuitive for finding a
#' NOEC (No-Observed-Effect Concentration).
#'
#' The standard ecotoxicology step-down procedure is:
#' 1. Test for a trend across all treatment groups vs. control.
#' 2. If significant, remove the highest dose and re-test.
#' 3. Repeat until the test is not significant. The highest dose in that
#'    non-significant test is the NOEC.
#'
#' This wrapper presents the results in that exact sequence.
#'
#' @param formula A formula of the form `response ~ group`.
#' @param data A data frame containing the variables in the formula.
#' @param test The trend test to use (e.g., "jonckheereTest"). Passed to the original function.
#' @param ... Other arguments to be passed to PMCMRplus::stepDownTrendTest (e.g., alternative).
#' @return An object of class `trendPMCMR`, with p-values and statistics ordered
#'   from the full dataset down to the smallest comparison (Control vs. T1).
#' @export
stepDownTrendTest_NOEC <- function(formula, data, test = "jonckheereTest", ...) {

  # --- Step 1: Call the original PMCMRplus function ---
  # This gets us all the necessary calculations, even if they are in the "wrong" order.
  original_result <- PMCMRplus::stepDownTrendTest(
    formula = formula,
    data = data,
    test = test,
    ...
  )

  # --- Step 2: Reverse the order of the results ---
  # The original function outputs results in a "step-up" manner. We simply
  # reverse the rows of the p-value and statistic matrices to get a "step-down" order.

  num_rows <- nrow(original_result$p.value)
  reverse_order_indices <- num_rows:1

  # Reverse the p-value matrix rows. `drop = FALSE` prevents R from simplifying
  # the matrix to a vector if it only has one column.
  p_reordered <- original_result$p.value[reverse_order_indices, , drop = FALSE]

  # Reverse the statistic matrix rows.
  stat_reordered <- original_result$statistic[reverse_order_indices, , drop = FALSE]

  # --- Step 3: Create meaningful row names for the new order ---
  # The original row names (e.g., "2", "3", "4") indicate the highest group included.
  # We can create more descriptive names for our step-down procedure.
  group_levels <- levels(data[[all.vars(formula)[2]]])
  new_rownames <- sapply(num_rows:1, function(i) {
    # i=num_rows is the full test, i=1 is the control vs lowest dose test.
    highest_group_name <- group_levels[i + 1]
    paste0("Control through ", highest_group_name)
  })

  rownames(p_reordered) <- new_rownames
  rownames(stat_reordered) <- new_rownames

  # --- Step 4: Build and return the new results object ---
  # We copy the original object and just replace the parts we've changed.
  new_result <- original_result
  new_result$p.value <- p_reordered
  new_result$statistic <- stat_reordered

  # Let's also update the method description to avoid confusion
  new_result$method <- paste("Re-ordered Step-down", original_result$method)

  return(new_result)
}
