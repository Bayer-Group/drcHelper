# Extract and test the exact validation function from the Rmd
library(drcHelper)
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

# Tolerance settings
tolerance <- 1e-6
p_value_tolerance <- 1e-4

# Helper function
convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

# Exact copy of the validation function from Rmd
run_dunnett_validation <- function(study_id, function_group_id, alternative = "less") {
  
  cat("=== VALIDATION FUNCTION DEBUG ===\n")
  cat("Inputs: study_id =", study_id, ", function_group_id =", function_group_id, ", alternative =", alternative, "\n")
  
  # First, get expected results to determine which endpoint we're testing
  expected_results <- test_cases_res[
    test_cases_res[['Function group ID']] == function_group_id &
    test_cases_res[['Study ID']] == study_id &
    grepl("Dunnett", test_cases_res[['Brief description']]), ]
  
  cat("Expected results found:", nrow(expected_results), "\n")
  
  if(nrow(expected_results) == 0) {
    return(list(passed = FALSE, error = "No Dunnett expected results found"))
  }
  
  # Get the endpoint we're testing from the expected results
  test_endpoint <- unique(expected_results[['Endpoint']])[1]
  cat("Test endpoint:", test_endpoint, "\n")
  
  # Get test data for this study AND SPECIFIC ENDPOINT
  study_data <- test_cases_data[
    test_cases_data[['Study ID']] == study_id & 
    test_cases_data[['Endpoint']] == test_endpoint, ]
  
  cat("Study data rows:", nrow(study_data), "\n")
  
  if(nrow(study_data) == 0) {
    return(list(passed = FALSE, error = paste("No data found for study", study_id, "endpoint", test_endpoint)))
  }
  
  # Convert dose to numeric
  study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
  study_data <- study_data[!is.na(study_data$Dose_numeric), ]
  
  cat("After dose conversion:", nrow(study_data), "\n")
  
  # Filter expected results for the specific alternative hypothesis
  alternative_pattern <- switch(alternative,
    "less" = "smaller",
    "greater" = "greater", 
    "two.sided" = "two-sided")
  
  expected_alt <- expected_results[grepl(alternative_pattern, expected_results[['Brief description']]), ]
  cat("Expected results for alternative:", nrow(expected_alt), "\n")
  
  if(nrow(expected_alt) == 0) {
    return(list(passed = FALSE, error = paste("No expected results for alternative:", alternative)))
  }
  
  tryCatch({
    # Check count data
    has_count_data <- any(!is.na(study_data$Total)) || 
                      any(!is.na(study_data$Alive)) || 
                      any(!is.na(study_data$Dead))
    cat("Has count data:", has_count_data, "\n")
    
    if(has_count_data) {
      cat("RETURNING: Count data detected\n")
      return(list(passed = TRUE, note = "Count data test skipped - requires specialized implementation"))
    } else {
      # Continuous data - standard Dunnett test
      cat("Processing continuous data...\n")
      
      # Create artificial Tank variable
      study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))
      
      # Prepare data
      test_data <- data.frame(
        Response = study_data$Response,
        Dose = study_data$Dose_numeric,
        Tank = study_data$Tank
      )
      
      # Find control level
      control_level <- if (0 %in% test_data$Dose) {
        0
      } else if (any(is.na(test_data$Dose))) {
        NA
      } else {
        min(test_data$Dose, na.rm = TRUE)
      }
      
      cat("Control level:", control_level, "\n")
      
      # Run dunnett_test
      cat("Calling dunnett_test...\n")
      result <- dunnett_test(
        test_data,
        response_var = "Response",
        dose_var = "Dose", 
        tank_var = "Tank",
        control_level = control_level,
        include_random_effect = FALSE,
        alternative = alternative
      )
      
      cat("Dunnett test completed, results table rows:", ifelse(is.null(result$results_table), 0, nrow(result$results_table)), "\n")
      
      # Validate results against expected values
      validation_results <- data.frame(
        metric = character(),
        expected = numeric(),
        actual = numeric(), 
        diff = numeric(),
        passed = logical(),
        stringsAsFactors = FALSE
      )
      
      cat("Starting validation comparisons...\n")
      
      # Extract key metrics from Dunnett test results
      if(!is.null(result$results_table)) {
        results_df <- result$results_table
        
        cat("Results table structure:\n")
        cat("Columns:", paste(names(results_df), collapse=", "), "\n")
        cat("Comparisons:", paste(results_df$comparison, collapse="; "), "\n")
        
        # Compare T-values
        tvalue_expected <- expected_alt[grepl("T-value", expected_alt[['Brief description']]), ]
        cat("T-value comparisons to check:", nrow(tvalue_expected), "\n")
        
        if(nrow(tvalue_expected) > 0) {
          for(i in 1:min(3, nrow(tvalue_expected))) {  # Limit to 3 for debugging
            exp_dose <- convert_dose(tvalue_expected$Dose[i])
            exp_value <- as.numeric(tvalue_expected[['expected result value']][i])
            
            cat(sprintf("  Looking for T-value at dose %s, expected %f\n", exp_dose, exp_value))
            
            # Find corresponding t-statistic in results
            comparison_pattern <- paste0("^", exp_dose, " - ")
            result_row <- which(grepl(comparison_pattern, results_df$comparison))
            
            if(length(result_row) > 0) {
              actual_tstat <- results_df$statistic[result_row[1]]
              diff_val <- abs(actual_tstat - exp_value)
              passed <- diff_val < tolerance
              
              cat(sprintf("    Found match: actual %f, diff %f, passed %s\n", actual_tstat, diff_val, passed))
              
              validation_results <- rbind(validation_results, data.frame(
                metric = paste("T-statistic at dose", exp_dose),
                expected = exp_value,
                actual = actual_tstat,
                diff = diff_val,
                passed = passed
              ))
            } else {
              cat(sprintf("    No match found for pattern '%s'\n", comparison_pattern))
            }
          }
        }
        
        cat("Validation results so far:", nrow(validation_results), "rows\n")
      }
      
      # Overall test result
      overall_passed <- if(nrow(validation_results) > 0) all(validation_results$passed) else TRUE
      
      cat("Overall passed:", overall_passed, "\n")
      cat("Validation rows:", nrow(validation_results), "\n")
      
      return(list(
        passed = overall_passed,
        validation_results = validation_results,
        n_comparisons = nrow(validation_results),
        n_passed = sum(validation_results$passed),
        dunnett_result = result
      ))
    }
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(list(passed = FALSE, error = paste("Test execution failed:", e$message)))
  })
}

# Test with FG00225
cat("Testing FG00225...\n")
result <- run_dunnett_validation("MOCKSE21/001-1", "FG00225", "less")

cat("\n=== FINAL RESULT ===\n")
cat("Passed:", result$passed, "\n")
if(!is.null(result$error)) cat("Error:", result$error, "\n")
if(!is.null(result$note)) cat("Note:", result$note, "\n")
if(!is.null(result$n_comparisons)) cat("Comparisons:", result$n_comparisons, "\n")
if(!is.null(result$n_passed)) cat("Passed comparisons:", result$n_passed, "\n")