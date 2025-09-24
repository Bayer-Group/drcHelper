# Simple test of multi-endpoint validation for FG00225
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

# Test the function directly
source("comprehensive_validation_functions.R")

cat("Testing FG00225 with correct Study ID...\n")
result <- run_dunnett_validation("MOCKSE21/001-1", "FG00225", alternative = "less")

cat("Result structure:\n")
str(result, max.level = 2)

if(!is.null(result$validation_results)) {
  cat("\nValidation results:\n")
  print(result$validation_results)
  
  cat("\nValidation results dimensions:", dim(result$validation_results), "\n")
} else {
  cat("\nNo validation results\n")
}

cat("\nOverall passed:", result$passed, "\n")
cat("Number of comparisons:", result$n_comparisons, "\n")
cat("Number passed:", result$n_passed, "\n")