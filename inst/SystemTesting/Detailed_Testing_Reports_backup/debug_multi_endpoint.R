# Test the multi-endpoint validation function with FG00225
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

# Load the function
source("comprehensive_validation_functions.R")

# Test FG00225 specifically
cat("Testing FG00225 (multi-endpoint)...\n")
result <- run_dunnett_validation("2019-IVA-001", "FG00225", alternative = "less")

cat("Result structure:\n")
str(result)

cat("\nEndpoints tested:", paste(result$endpoints_tested, collapse = ", "), "\n")
cat("Overall passed:", result$passed, "\n")
cat("Total validations:", result$n_comparisons, "\n")
cat("Passed validations:", result$n_passed, "\n")

if(!is.null(result$validation_results) && nrow(result$validation_results) > 0) {
  cat("\nValidation results summary:\n")
  print(result$validation_results)
} else {
  cat("\nNo validation results to display\n")
}