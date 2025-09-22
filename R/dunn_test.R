#' Dunn's Multiple Comparison Test
#'
#' Performs Dunn's multiple comparison test for comparing treatment groups against a control
#' after a significant Kruskal-Wallis test. This is a wrapper around PMCMRplus::kwManyOneDunnTest
#' that provides consistent output structure with other drcHelper test functions.
#'
#' @param data A data frame containing the response and grouping variables
#' @param response_var Character string specifying the name of the response variable
#' @param dose_var Character string specifying the name of the dose/treatment variable  
#' @param control_level The control level (default: 0)
#' @param alternative Character string specifying the alternative hypothesis.
#'   Must be one of "less", "greater", or "two.sided" (default: "less")
#' @param p_adjust_method Character string specifying the p-value adjustment method
#'   (default: "holm"). See p.adjust.methods for available methods
#' @param alpha Significance level (default: 0.05)
#' @param include_kruskal Logical indicating whether to include Kruskal-Wallis test results
#'   (default: TRUE)
#'
#' @return A list of class "dunn_test_result" containing:
#' \describe{
#'   \item{results_table}{Data frame with comparison results including z-values and p-values}
#'   \item{kruskal_wallis}{Kruskal-Wallis test results (if include_kruskal = TRUE)}
#'   \item{noec}{No Observed Effect Concentration}
#'   \item{noec_message}{Description of NOEC determination}
#'   \item{model_type}{Description of the statistical method used}
#'   \item{control_level}{The control level used}
#'   \item{alpha}{Significance level used}
#'   \item{alternative}{Alternative hypothesis tested}
#'   \item{p_adjust_method}{P-value adjustment method used}
#' }
#'
#' @note This function uses PMCMRplus::kwManyOneDunnTest which produces equivalent results
#'   to DescTools::DunnTest. Both implementations use the same underlying statistical
#'   methodology for Dunn's post-hoc test following Kruskal-Wallis.
#'
#' @examples
#' \dontrun{
#' # Example data
#' Rate <- c(0,0,0,0,0,0,
#'           0.0448,0.0448,0.0448,0.0448,
#'           0.132,0.132,0.132,0.132)
#' y <- c(0.131,0.117,0.130,0.122,0.127,0.128,
#'        0.122,0.126,0.128,0.116,
#'        0.090,0.102,0.107,0.099)
#' test_data <- data.frame(Rate = Rate, Response = y)
#' 
#' # Run Dunn's test
#' result <- dunn_test(test_data, response_var = "Response", 
#'                     dose_var = "Rate", control_level = 0, 
#'                     alternative = "less")
#' }
#'
#' @export
dunn_test <- function(data, response_var, dose_var, control_level = 0, 
                      alternative = "less", p_adjust_method = "holm", 
                      alpha = 0.05, include_kruskal = TRUE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!response_var %in% names(data)) {
    stop(paste("Response variable", response_var, "not found in data"))
  }
  
  if (!dose_var %in% names(data)) {
    stop(paste("Dose variable", dose_var, "not found in data"))
  }
  
  if (!alternative %in% c("less", "greater", "two.sided")) {
    stop("alternative must be one of 'less', 'greater', or 'two.sided'")
  }
  
  # Ensure required packages are available
  if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
    stop("PMCMRplus package is required but not installed")
  }
  
  # Prepare data
  test_data <- data[, c(response_var, dose_var)]
  names(test_data) <- c("Response", "Dose")
  
  # Convert dose to factor for proper ordering
  test_data$Dose <- as.factor(test_data$Dose)
  
  # Check if control level exists in data
  if (!control_level %in% levels(test_data$Dose)) {
    stop(paste("Control level", control_level, "not found in dose data"))
  }
  
  start_time <- Sys.time()
  
  # Run Kruskal-Wallis test first (optional but informative)
  kruskal_result <- NULL
  if (include_kruskal) {
    kruskal_result <- kruskal.test(Response ~ Dose, data = test_data)
  }
  
  # Run Dunn's multiple comparison test using PMCMRplus
  # Note: PMCMRplus::kwManyOneDunnTest gives equivalent results to DescTools::DunnTest
  dunn_result <- PMCMRplus::kwManyOneDunnTest(
    Response ~ Dose, 
    data = test_data,
    alternative = alternative,
    p.adjust.method = p_adjust_method
  )
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Extract results and create standardized output
  if (is.matrix(dunn_result$p.value)) {
    p_values <- dunn_result$p.value[1, ]
    comparisons <- colnames(dunn_result$p.value)
  } else {
    p_values <- dunn_result$p.value
    comparisons <- names(dunn_result$p.value)
  }
  
  # Get statistic values (z-values)
  if (is.matrix(dunn_result$statistic)) {
    z_values <- dunn_result$statistic[1, ]
  } else {
    z_values <- dunn_result$statistic
  }
  
  # Create results table
  results_table <- data.frame(
    comparison = comparisons,
    z_value = as.numeric(z_values),
    p.value = as.numeric(p_values),
    significant = p_values < alpha,
    stringsAsFactors = FALSE
  )
  
  # Calculate means by dose level for additional information
  dose_means <- aggregate(Response ~ Dose, data = test_data, FUN = mean)
  names(dose_means) <- c("dose", "mean_response")
  
  # Add mean responses to results table
  results_table$control_mean <- dose_means$mean_response[dose_means$dose == control_level]
  
  # Add treatment means
  treatment_doses <- gsub(paste0(control_level, "$"), "", results_table$comparison)
  treatment_doses <- gsub("^.*-\\s*", "", treatment_doses)
  
  results_table$treatment_mean <- sapply(treatment_doses, function(dose) {
    mean_val <- dose_means$mean_response[dose_means$dose == dose]
    if (length(mean_val) == 0) NA else mean_val
  })
  
  # Determine NOEC
  significant_comparisons <- results_table[results_table$significant, ]
  
  if (nrow(significant_comparisons) == 0) {
    noec <- max(as.numeric(as.character(test_data$Dose)))
    noec_message <- "No significant effects detected. NOEC is the highest tested dose."
  } else {
    # Find the lowest significant dose
    significant_doses <- sapply(significant_comparisons$comparison, function(comp) {
      dose_str <- gsub(paste0(control_level, "$"), "", comp)
      dose_str <- gsub("^.*-\\s*", "", dose_str)
      as.numeric(dose_str)
    })
    
    lowest_significant <- min(significant_doses, na.rm = TRUE)
    
    # NOEC is the highest dose below the lowest significant dose
    all_doses <- sort(as.numeric(as.character(unique(test_data$Dose))))
    noec_candidates <- all_doses[all_doses < lowest_significant]
    
    if (length(noec_candidates) == 0) {
      noec <- control_level
      noec_message <- "Lowest tested dose shows significant effect. NOEC equals control level."
    } else {
      noec <- max(noec_candidates)
      noec_message <- paste("NOEC determined as highest non-significant dose:", noec)
    }
  }
  
  # Create result object
  result <- list(
    results_table = results_table,
    kruskal_wallis = kruskal_result,
    noec = noec,
    noec_message = noec_message,
    model_type = paste("Dunn's multiple comparison test with", p_adjust_method, "adjustment"),
    control_level = control_level,
    alpha = alpha,
    alternative = alternative,
    p_adjust_method = p_adjust_method,
    execution_time = execution_time,
    dose_means = dose_means
  )
  
  class(result) <- "dunn_test_result"
  
  return(result)
}

#' Print method for dunn_test_result
#' @param x A dunn_test_result object
#' @param ... Additional arguments (not used)
#' @export
print.dunn_test_result <- function(x, ...) {
  cat("Dunn's Multiple Comparison Test Results\n")
  cat("=======================================\n\n")
  
  if (!is.null(x$kruskal_wallis)) {
    cat("Kruskal-Wallis test:\n")
    cat("  H-statistic =", round(x$kruskal_wallis$statistic, 4), "\n")
    cat("  p-value =", format(x$kruskal_wallis$p.value, scientific = TRUE, digits = 4), "\n\n")
  }
  
  cat("Multiple comparisons (vs. control =", x$control_level, "):\n")
  cat("Alternative hypothesis:", x$alternative, "\n")
  cat("P-value adjustment method:", x$p_adjust_method, "\n\n")
  
  print(x$results_table)
  
  cat("\n")
  cat("NOEC:", x$noec, "\n")
  cat("NOEC message:", x$noec_message, "\n")
  cat("Significance level:", x$alpha, "\n")
}