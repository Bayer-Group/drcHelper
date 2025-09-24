# Debug why tests are passing without actually running
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

# Helper function to convert European decimal notation
convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

# Test specifically FG00225 with detailed output
debug_validation <- function(study_id, function_group_id, alternative = "less") {
  cat("=== DEBUGGING VALIDATION FUNCTION ===\n")
  cat("Study ID:", study_id, "\n")
  cat("Function Group ID:", function_group_id, "\n")
  cat("Alternative:", alternative, "\n\n")
  
  # Step 1: Get expected results
  expected_results <- test_cases_res[
    test_cases_res[['Function group ID']] == function_group_id &
    test_cases_res[['Study ID']] == study_id &
    grepl("Dunnett", test_cases_res[['Brief description']]), ]
  
  cat("Step 1 - Expected results found:", nrow(expected_results), "\n")
  if(nrow(expected_results) == 0) {
    return(list(passed = FALSE, error = "No Dunnett expected results found"))
  }
  
  # Step 2: Get test endpoint
  test_endpoint <- unique(expected_results[['Endpoint']])[1]
  cat("Step 2 - Test endpoint:", test_endpoint, "\n")
  
  # Step 3: Get study data for specific endpoint
  study_data <- test_cases_data[
    test_cases_data[['Study ID']] == study_id & 
    test_cases_data[['Endpoint']] == test_endpoint, ]
  
  cat("Step 3 - Study data rows:", nrow(study_data), "\n")
  if(nrow(study_data) == 0) {
    return(list(passed = FALSE, error = paste("No data found for endpoint", test_endpoint)))
  }
  
  # Step 4: Convert doses
  study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
  study_data <- study_data[!is.na(study_data$Dose_numeric), ]
  cat("Step 4 - Data after dose conversion:", nrow(study_data), "\n")
  cat("         Dose range:", min(study_data$Dose_numeric), "to", max(study_data$Dose_numeric), "\n")
  
  # Step 5: Filter expected results by alternative
  alternative_pattern <- switch(alternative,
    "less" = "smaller",
    "greater" = "greater", 
    "two.sided" = "two-sided")
  
  expected_alt <- expected_results[grepl(alternative_pattern, expected_results[['Brief description']]), ]
  cat("Step 5 - Expected results for alternative '", alternative, "':", nrow(expected_alt), "\n")
  
  if(nrow(expected_alt) == 0) {
    return(list(passed = FALSE, error = paste("No expected results for alternative:", alternative)))
  }
  
  # Step 6: Check count data
  has_count_data <- any(!is.na(study_data$Total)) || 
                    any(!is.na(study_data$Alive)) || 
                    any(!is.na(study_data$Dead))
  cat("Step 6 - Has count data:", has_count_data, "\n")
  
  if(has_count_data) {
    cat("RESULT: Returning early - count data detected\n")
    return(list(passed = TRUE, note = "Count data test skipped - requires specialized implementation"))
  }
  
  # Step 7: Prepare for Dunnett test
  cat("Step 7 - Preparing for Dunnett test...\n")
  
  # Create Tank variable
  study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))
  
  test_data <- data.frame(
    Response = study_data$Response,
    Dose = study_data$Dose_numeric,
    Tank = study_data$Tank
  )
  
  control_level <- if (0 %in% test_data$Dose) {
    0
  } else if (any(is.na(test_data$Dose))) {
    NA
  } else {
    min(test_data$Dose, na.rm = TRUE)
  }
  
  cat("         Control level:", control_level, "\n")
  cat("         Test data rows:", nrow(test_data), "\n")
  
  # Step 8: Check if dunnett_test function exists and try to call it
  cat("Step 8 - Checking dunnett_test function...\n")
  
  if (!exists("dunnett_test")) {
    cat("ERROR: dunnett_test function not found!\n")
    return(list(passed = FALSE, error = "dunnett_test function not available"))
  }
  
  cat("         Function exists, attempting call...\n")
  
  tryCatch({
    result <- dunnett_test(
      test_data,
      response_var = "Response",
      dose_var = "Dose", 
      tank_var = "Tank",
      control_level = control_level,
      include_random_effect = FALSE,
      alternative = alternative
    )
    
    cat("         Dunnett test completed successfully!\n")
    cat("         Results table rows:", ifelse(is.null(result$results_table), 0, nrow(result$results_table)), "\n")
    
    # Continue with validation...
    return(list(passed = TRUE, note = "Dunnett test executed", result = result))
    
  }, error = function(e) {
    cat("ERROR in dunnett_test:", e$message, "\n")
    return(list(passed = FALSE, error = paste("Dunnett test failed:", e$message)))
  })
}

# Test with FG00225
cat("Testing FG00225 (BRSOL Plant Tests)...\n")
result <- debug_validation("MOCKSE21/001-1", "FG00225", "less")
cat("\nFINAL RESULT:\n")
cat("Passed:", result$passed, "\n")
if(!is.null(result$error)) cat("Error:", result$error, "\n")
if(!is.null(result$note)) cat("Note:", result$note, "\n")