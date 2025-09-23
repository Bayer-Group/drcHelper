# Investigate multiple studies with multiple endpoints scenario
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

cat("=== INVESTIGATING MULTIPLE STUDIES WITH MULTIPLE ENDPOINTS ===\n\n")

# Check what studies have multiple endpoints
cat("1. Analyzing all studies for multi-endpoint capability:\n")

# Get all unique study-endpoint combinations
study_endpoint_combos <- test_cases_data[, c("Study ID", "Endpoint")]
study_endpoint_combos <- unique(study_endpoint_combos)

# Count endpoints per study
endpoint_counts <- table(study_endpoint_combos$`Study ID`)
multi_endpoint_studies <- names(endpoint_counts[endpoint_counts > 1])

cat("Studies with multiple endpoints:\n")
for(study in multi_endpoint_studies) {
  endpoints <- unique(study_endpoint_combos$Endpoint[study_endpoint_combos$`Study ID` == study])
  cat("-", study, ":", length(endpoints), "endpoints ->", paste(endpoints, collapse = ", "), "\n")
}

cat("\n2. Checking for expected results (test_cases_res) coverage:\n")

# Check which multi-endpoint studies have expected results
multi_endpoint_with_expected <- c()
for(study in multi_endpoint_studies) {
  expected_count <- nrow(test_cases_res[test_cases_res$`Study ID` == study, ])
  if(expected_count > 0) {
    multi_endpoint_with_expected <- c(multi_endpoint_with_expected, study)
    cat("-", study, ":", expected_count, "expected results\n")
  }
}

cat("\n3. Testing current validation function with multiple studies:\n")

# Load our validation function
source("comprehensive_validation_functions.R")

# Test each multi-endpoint study
for(study in multi_endpoint_with_expected) {
  cat("\n--- Testing Study:", study, "---\n")
  
  # Find function group ID for this study
  fg_id <- unique(test_cases_res$`Function group ID`[test_cases_res$`Study ID` == study])
  
  if(length(fg_id) > 0) {
    cat("Function Group ID(s):", paste(fg_id, collapse = ", "), "\n")
    
    # Test with first function group ID
    result <- tryCatch({
      run_dunnett_validation(study, fg_id[1], alternative = "less")
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
      return(list(passed = FALSE, error = e$message))
    })
    
    if(!is.null(result$endpoints_tested)) {
      cat("Endpoints tested:", paste(result$endpoints_tested, collapse = ", "), "\n")
      cat("Total validations:", ifelse(is.null(result$n_comparisons), 0, result$n_comparisons), "\n")
      cat("Passed validations:", ifelse(is.null(result$n_passed), 0, result$n_passed), "\n")
      cat("Overall result:", ifelse(result$passed, "✅ PASSED", "❌ FAILED"), "\n")
    }
  } else {
    cat("No function group ID found for this study\n")
  }
}

cat("\n=== SUMMARY ===\n")
cat("Total studies with multiple endpoints:", length(multi_endpoint_studies), "\n")
cat("Studies with expected results:", length(multi_endpoint_with_expected), "\n")
cat("\nMulti-endpoint studies ready for validation:\n")
for(study in multi_endpoint_with_expected) {
  endpoints <- unique(study_endpoint_combos$Endpoint[study_endpoint_combos$`Study ID` == study])
  cat("-", study, ":", paste(endpoints, collapse = ", "), "\n")
}