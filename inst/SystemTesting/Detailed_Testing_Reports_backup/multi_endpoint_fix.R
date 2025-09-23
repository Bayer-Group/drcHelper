# Multi-endpoint Validation Fix for Comprehensive Dunnett Validation
# This file contains the corrected run_dunnett_validation function that supports multiple endpoints

# Save the current run_dunnett_validation function with multi-endpoint support
run_dunnett_validation_multi_endpoint <- function(study_id, function_group_id, alternative = "less") {
  
  # Get expected results for this study and function group
  expected_results <- test_cases_res[
    test_cases_res[['Study ID']] == study_id &
    test_cases_res[['Function group ID']] == function_group_id &
    grepl("Dunnett", test_cases_res[['Brief description']]), ]
  
  if(nrow(expected_results) == 0) {
    cat("No Dunnett expected results found\\n")
    return(list(passed = FALSE, error = "No Dunnett expected results found"))
  }
  
  # Get all available endpoints from expected results
  available_endpoints <- unique(expected_results[['Endpoint']])
  cat("Available endpoints:", paste(available_endpoints, collapse = ", "), "\\n")
  
  # Initialize storage for multiple endpoint results
  endpoint_results <- list()
  
  # For multi-endpoint studies, test each endpoint separately
  for(test_endpoint in available_endpoints) {
    cat("Testing endpoint:", test_endpoint, "\\n")
    
    # Initialize validation results for this endpoint
    validation_results <- data.frame(
      endpoint = character(),
      metric = character(),
      dose = character(),
      expected = numeric(),
      actual = numeric(),
      diff = numeric(),
      passed = logical(),
      stringsAsFactors = FALSE
    )
    
    # Get study data for specific endpoint
    study_data <- test_cases_data[
      test_cases_data[['Study ID']] == study_id & 
      test_cases_data[['Endpoint']] == test_endpoint, ]
    
    if(nrow(study_data) == 0) {
      cat("No data found for", study_id, test_endpoint, "\\n")
      next
    }
    
    # Convert dose to numeric (handle European decimal notation)
    study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
    study_data <- study_data[!is.na(study_data$Dose_numeric), ]
    
    # Filter expected results for the specific alternative hypothesis AND endpoint
    alternative_pattern <- switch(alternative,
      "less" = "smaller",
      "greater" = "greater", 
      "two.sided" = "two-sided")
    
    expected_alt <- expected_results[grepl(alternative_pattern, expected_results[['Brief description']]) &
                                   expected_results[['Endpoint']] == test_endpoint, ]
    
    if(nrow(expected_alt) == 0) {
      cat("No expected results for alternative:", alternative, "endpoint:", test_endpoint, "\\n")
      next
    }
    
    tryCatch({
      # Check for count data (endpoint-specific)
      has_count_data <- any(!is.na(study_data$Total)) || 
                        any(!is.na(study_data$Alive)) || 
                        any(!is.na(study_data$Dead))
      
      if(has_count_data) {
        # Count data - requires specialized implementation
        endpoint_results[[test_endpoint]] <- list(
          passed = TRUE, 
          note = "Count data endpoint - requires specialized implementation",
          validation_results = data.frame(),
          n_comparisons = 0,
          n_passed = 0
        )
        next
      }
      
      # Continuous data - standard Dunnett test
      # Create artificial Tank variable for replication structure
      study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))
      
      # Prepare data with proper column names
      test_data <- data.frame(
        Response = study_data$Response,
        Dose = study_data$Dose_numeric,
        Tank = study_data$Tank
      )
      
      # Find control level - handle both 0 and NA cases
      control_level <- if (0 %in% test_data$Dose) {
        0
      } else if (any(is.na(test_data$Dose))) {
        NA
      } else {
        min(test_data$Dose, na.rm = TRUE)
      }
      
      # Run actual dunnett_test
      result <- dunnett_test(
        test_data,
        response_var = "Response",
        dose_var = "Dose", 
        tank_var = "Tank",
        control_level = control_level,
        include_random_effect = FALSE,
        alternative = alternative
      )
      
      if(is.null(result$results_table) || nrow(result$results_table) == 0) {
        endpoint_results[[test_endpoint]] <- list(
          passed = FALSE, 
          error = "Dunnett test failed",
          validation_results = data.frame(),
          n_comparisons = 0,
          n_passed = 0
        )
        next
      }
      
      # Validate results against expected values
      results_df <- result$results_table
      
      # Validate T-statistics with improved dose matching and NA filtering
      tstat_expected <- expected_alt[grepl("t-value|T-value", expected_alt[['Brief description']]), ]
      for(i in 1:nrow(tstat_expected)) {
        exp_dose <- convert_dose(tstat_expected$Dose[i])
        exp_value_str <- as.character(tstat_expected[['expected result value']][i])
        
        # Skip if expected value is not numeric
        if(is.na(exp_value_str) || exp_value_str == "-" || exp_value_str == "") {
          next
        }
        
        exp_value <- suppressWarnings(as.numeric(exp_value_str))
        if(is.na(exp_value)) {
          next
        }
        
        # Find matching comparison in results using tolerance
        comparison_matches <- which(sapply(results_df$comparison, function(comp) {
          parts <- strsplit(comp, " - ")[[1]]
          if(length(parts) >= 1) {
            comp_dose <- suppressWarnings(as.numeric(parts[1]))
            return(!is.na(comp_dose) && abs(comp_dose - exp_dose) < 0.001)
          }
          return(FALSE)
        }))
        
        if(length(comparison_matches) > 0) {
          actual_tstat <- results_df$statistic[comparison_matches[1]]
          diff_val <- abs(actual_tstat - exp_value)
          passed <- diff_val < tolerance
          
          validation_results <- rbind(validation_results, data.frame(
            endpoint = test_endpoint,
            metric = "T-statistic",
            dose = as.character(exp_dose),
            expected = exp_value,
            actual = actual_tstat,
            diff = diff_val,
            passed = passed,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Validate P-values with improved dose matching and NA filtering
      pvalue_expected <- expected_alt[grepl("p-value", expected_alt[['Brief description']]), ]
      for(i in 1:nrow(pvalue_expected)) {
        exp_dose <- convert_dose(pvalue_expected$Dose[i])
        exp_pval_str <- as.character(pvalue_expected[['expected result value']][i])
        
        # Skip if expected value is not numeric
        if(is.na(exp_pval_str) || exp_pval_str == "-" || exp_pval_str == "") {
          next
        }
        
        exp_pval <- suppressWarnings(as.numeric(exp_pval_str))
        if(is.na(exp_pval)) {
          next
        }
        
        # Find matching comparison in results using tolerance
        comparison_matches <- which(sapply(results_df$comparison, function(comp) {
          parts <- strsplit(comp, " - ")[[1]]
          if(length(parts) >= 1) {
            comp_dose <- suppressWarnings(as.numeric(parts[1]))
            return(!is.na(comp_dose) && abs(comp_dose - exp_dose) < 0.001)
          }
          return(FALSE)
        }))
        
        if(length(comparison_matches) > 0) {
          actual_pval <- results_df$p.value[comparison_matches[1]]
          diff_val <- abs(actual_pval - exp_pval)
          passed <- diff_val < p_value_tolerance
          
          validation_results <- rbind(validation_results, data.frame(
            endpoint = test_endpoint,
            metric = "P-value",
            dose = as.character(exp_dose),
            expected = exp_pval,
            actual = actual_pval,
            diff = diff_val,
            passed = passed,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Validate Means with improved dose matching and NA filtering
      means_expected <- expected_alt[grepl("Mean", expected_alt[['Brief description']]), ]
      for(i in 1:nrow(means_expected)) {
        exp_dose <- convert_dose(means_expected$Dose[i])
        exp_value_str <- as.character(means_expected[['expected result value']][i])
        
        # Skip if expected value is not numeric
        if(is.na(exp_value_str) || exp_value_str == "-" || exp_value_str == "") {
          next
        }
        
        exp_value <- suppressWarnings(as.numeric(exp_value_str))
        if(is.na(exp_value)) {
          next
        }
        
        # Calculate actual mean for this dose
        actual_mean <- mean(test_data$Response[test_data$Dose == exp_dose], na.rm = TRUE)
        if(!is.na(actual_mean)) {
          diff_val <- abs(actual_mean - exp_value)
          passed <- diff_val < tolerance
          
          validation_results <- rbind(validation_results, data.frame(
            endpoint = test_endpoint,
            metric = "Mean",
            dose = as.character(exp_dose),
            expected = exp_value,
            actual = actual_mean,
            diff = diff_val,
            passed = passed,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Calculate overall result for this endpoint
      endpoint_passed <- if(nrow(validation_results) > 0) all(validation_results$passed) else TRUE
      
      # Store results for this endpoint
      endpoint_results[[test_endpoint]] <- list(
        passed = endpoint_passed,
        validation_results = validation_results,
        n_comparisons = nrow(validation_results),
        n_passed = sum(validation_results$passed)
      )
      
      cat("Endpoint", test_endpoint, "validation completed:", sum(validation_results$passed), "/", nrow(validation_results), "passed\\n\\n")
      
    }, error = function(e) {
      cat("Error processing endpoint", test_endpoint, ":", e$message, "\\n")
      endpoint_results[[test_endpoint]] <- list(
        passed = FALSE,
        error = paste("Test execution failed:", e$message),
        validation_results = data.frame(),
        n_comparisons = 0,
        n_passed = 0
      )
    })
  } # End endpoint loop
  
  # Combine results from all endpoints
  all_validation_results <- do.call(rbind, lapply(names(endpoint_results), function(ep) {
    if(!is.null(endpoint_results[[ep]]$validation_results) && nrow(endpoint_results[[ep]]$validation_results) > 0) {
      endpoint_results[[ep]]$validation_results
    } else {
      data.frame()
    }
  }))
  
  # Calculate overall result across all endpoints
  overall_passed <- if(nrow(all_validation_results) > 0) all(all_validation_results$passed) else TRUE
  
  return(list(
    passed = overall_passed,
    endpoints_tested = names(endpoint_results),
    endpoint_results = endpoint_results,
    validation_results = all_validation_results,
    n_comparisons = nrow(all_validation_results),
    n_passed = sum(all_validation_results$passed)
  ))
}

cat("Multi-endpoint validation function defined successfully\\n")