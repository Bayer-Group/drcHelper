# Debug the rbind issue by testing each function group individually
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

# Load the function
source("comprehensive_validation_functions.R")

# Test each function group
function_groups <- list(
  list(id = "FG00220", name = "Plant height bioassay - DUNNETT", study = "MOCK0065"),
  list(id = "FG00221", name = "Shoot dry weight bioassay - DUNNETT", study = "MOCK08/15-001"),
  list(id = "FG00222", name = "Repellency bioassay - DUNNETT", study = "MOCK08/15-001"),
  list(id = "FG00225", name = "Plant bioassay, two endpoints - DUNNETT", study = "MOCKSE21/001-1")
)

for(i in 1:length(function_groups)) {
  fg <- function_groups[[i]]
  cat("\n=== Testing", fg$id, "===\n")
  
  tryCatch({
    result <- run_dunnett_validation(fg$study, fg$id, alternative = "less")
    cat("SUCCESS: Passed =", result$passed, ", Comparisons =", result$n_comparisons, ", Passed =", result$n_passed, "\n")
    cat("Endpoints tested:", paste(result$endpoints_tested, collapse = ", "), "\n")
    
    # Check validation_results structure
    if(!is.null(result$validation_results)) {
      cat("Validation results dimensions:", dim(result$validation_results), "\n")
    } else {
      cat("Validation results: NULL\n")
    }
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    cat("Error class:", class(e), "\n")
  })
}