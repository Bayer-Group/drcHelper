# Find all function groups with Dunnett tests across all studies
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

cat("=== FINDING ALL DUNNETT FUNCTION GROUPS ===\n\n")

# Find all function groups with Dunnett tests
dunnett_fg <- test_cases_res[grepl("Dunnett", test_cases_res$`Brief description`), ]

cat("Function Groups with Dunnett tests:\n")
dunnett_fg_summary <- dunnett_fg[, c("Function group ID", "Study ID")]
dunnett_fg_summary <- unique(dunnett_fg_summary)
dunnett_fg_summary <- dunnett_fg_summary[order(dunnett_fg_summary$`Study ID`, dunnett_fg_summary$`Function group ID`), ]

print(dunnett_fg_summary)

cat("\nGrouped by Study ID:\n")
for(study in unique(dunnett_fg_summary$`Study ID`)) {
  fg_ids <- dunnett_fg_summary$`Function group ID`[dunnett_fg_summary$`Study ID` == study]
  cat("-", study, ":", paste(fg_ids, collapse = ", "), "\n")
  
  # Check endpoints for each FG in this study
  for(fg in fg_ids) {
    endpoints <- unique(dunnett_fg$Endpoint[dunnett_fg$`Function group ID` == fg & 
                                           dunnett_fg$`Study ID` == study])
    cat("  ", fg, "-> Endpoints:", paste(endpoints, collapse = ", "), "\n")
  }
}

cat("\n=== TESTING MULTIPLE STUDIES WITH MULTIPLE ENDPOINTS ===\n")

# Load our validation function
source("comprehensive_validation_functions.R")

# Test each Dunnett function group
all_results <- list()

for(i in 1:nrow(dunnett_fg_summary)) {
  study <- dunnett_fg_summary$`Study ID`[i]
  fg_id <- dunnett_fg_summary$`Function group ID`[i]
  
  cat("\n--- Testing", study, "/", fg_id, "---\n")
  
  result <- tryCatch({
    run_dunnett_validation(study, fg_id, alternative = "less")
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(list(passed = FALSE, error = e$message))
  })
  
  if(!is.null(result$endpoints_tested)) {
    cat("Endpoints tested:", paste(result$endpoints_tested, collapse = ", "), "\n")
    cat("Total validations:", ifelse(is.null(result$n_comparisons), 0, result$n_comparisons), "\n")
    cat("Passed validations:", ifelse(is.null(result$n_passed), 0, result$n_passed), "\n")
    cat("Overall result:", ifelse(result$passed, "✅ PASSED", "❌ FAILED"), "\n")
    
    # Store result
    all_results[[paste(study, fg_id, sep = "_")]] <- result
  }
}

cat("\n=== COMPREHENSIVE SUMMARY ===\n")
total_studies <- length(unique(dunnett_fg_summary$`Study ID`))
total_function_groups <- nrow(dunnett_fg_summary)
successful_tests <- sum(sapply(all_results, function(x) x$passed))
total_validations <- sum(sapply(all_results, function(x) ifelse(is.null(x$n_comparisons), 0, x$n_comparisons)))
passed_validations <- sum(sapply(all_results, function(x) ifelse(is.null(x$n_passed), 0, x$n_passed)))

cat("Studies with Dunnett tests:", total_studies, "\n")
cat("Total Dunnett function groups:", total_function_groups, "\n")
cat("Successful function group tests:", successful_tests, "/", total_function_groups, "\n")
cat("Total individual validations:", total_validations, "\n")  
cat("Passed individual validations:", passed_validations, "/", total_validations, "\n")
cat("Overall success rate:", round(100 * passed_validations / total_validations, 1), "%\n")

# Check for multi-endpoint studies specifically
cat("\nMulti-endpoint studies analysis:\n")
multi_endpoint_studies <- c()
for(study in unique(dunnett_fg_summary$`Study ID`)) {
  study_fgs <- dunnett_fg_summary$`Function group ID`[dunnett_fg_summary$`Study ID` == study]
  
  total_endpoints <- c()
  for(fg in study_fgs) {
    if(paste(study, fg, sep = "_") %in% names(all_results)) {
      endpoints <- all_results[[paste(study, fg, sep = "_")]]$endpoints_tested
      total_endpoints <- c(total_endpoints, endpoints)
    }
  }
  
  unique_endpoints <- unique(total_endpoints)
  if(length(unique_endpoints) > 1) {
    multi_endpoint_studies <- c(multi_endpoint_studies, study)
    cat("-", study, ":", length(study_fgs), "function groups covering", length(unique_endpoints), "endpoints ->", paste(unique_endpoints, collapse = ", "), "\n")
  }
}

cat("\nMultiple studies with multiple endpoints capability:", length(multi_endpoint_studies) > 1, "\n")
if(length(multi_endpoint_studies) > 1) {
  cat("✅ CONFIRMED: Package handles multiple studies, each with multiple endpoints\n")
} else {
  cat("⚠️ LIMITED: Only single study with multiple endpoints confirmed\n")
}