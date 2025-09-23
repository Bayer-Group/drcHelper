# Enhanced Validation Function with Detailed Row-by-Row Results
# =============================================================

# Function to get detailed validation results with individual comparisons
run_detailed_dunnett_validation <- function(study_id, function_group_id, alternative = "less") {
  
  # Get test cases for this study/function group
  test_cases <- test_cases_res[test_cases_res$`Study ID` == study_id & 
                              test_cases_res$`Function group ID` == function_group_id, ]
  
  if(nrow(test_cases) == 0) {
    return(list(
      passed = FALSE, 
      error = "No test cases found",
      detailed_results = data.frame(),
      endpoints_tested = c()
    ))
  }
  
  # Get available endpoints
  endpoints <- unique(test_cases$Endpoint)
  cat("Available endpoints:", paste(endpoints, collapse = ", "), "\n")
  
  detailed_results <- data.frame()
  all_endpoints_passed <- TRUE
  total_comparisons <- 0
  total_passed <- 0
  
  for(endpoint in endpoints) {
    cat("Testing endpoint:", endpoint, "\n")
    
    endpoint_result <- tryCatch({
      
      # Get test data for this endpoint
      test_data <- test_cases_data[test_cases_data$`Study ID` == study_id & 
                                  test_cases_data$`Function group ID` == function_group_id &
                                  test_cases_data$Endpoint == endpoint, ]
      
      if(nrow(test_data) == 0) {
        stop("No test data found for endpoint")
      }
      
      # Get expected results for this endpoint  
      expected_results <- test_cases[test_cases$Endpoint == endpoint, ]
      
      # Convert doses if needed
      test_data$dose_converted <- convert_dose(test_data$dose)
      
      # Fit model
      if(length(unique(test_data$vari)) == 1) {
        cat("Fitting linear model with homoscedastic errors\n")
        model <- lm(response ~ factor(dose_converted), data = test_data)
      } else {
        cat("Fitting linear model with heteroscedastic errors\n") 
        weights <- 1 / test_data$vari
        model <- lm(response ~ factor(dose_converted), data = test_data, weights = weights)
      }
      
      # Get dose levels (excluding control)
      dose_levels <- sort(unique(test_data$dose_converted))
      treatment_doses <- dose_levels[dose_levels != 0]
      
      if(length(treatment_doses) == 0) {
        stop("No treatment doses found")
      }
      
      # Perform Dunnett test
      library(multcomp)
      dose_factor <- factor(test_data$dose_converted)
      
      # Create contrast matrix for Dunnett test
      contrast_names <- paste0(treatment_doses, " - 0")
      dunnett_test <- glht(model, linfct = mcp(`factor(dose_converted)` = "Dunnett"), alternative = alternative)
      dunnett_summary <- summary(dunnett_test)
      
      # Extract results
      t_values <- dunnett_summary$test$tstat
      p_values <- dunnett_summary$test$pvalues
      
      # Create detailed comparison table
      endpoint_detailed <- data.frame(
        Study_ID = study_id,
        Function_Group = function_group_id,
        Endpoint = endpoint,
        Comparison = contrast_names,
        Actual_T_Value = round(t_values, 6),
        Actual_P_Value = round(p_values, 6),
        Expected_T_Value = NA,
        Expected_P_Value = NA,
        T_Match = FALSE,
        P_Match = FALSE,
        stringsAsFactors = FALSE
      )
      
      # Match with expected results
      endpoint_passed <- 0
      endpoint_total <- 0
      
      for(i in 1:length(contrast_names)) {
        comparison <- contrast_names[i]
        actual_t <- t_values[i]
        actual_p <- p_values[i]
        
        # Find matching expected results
        # Look for t-value results
        t_expected_rows <- expected_results[grepl("t-value", expected_results$`Brief description`) & 
                                          grepl(alternative, expected_results$`Brief description`), ]
        
        p_expected_rows <- expected_results[grepl("p-value", expected_results$`Brief description`) & 
                                          grepl(alternative, expected_results$`Brief description`) &
                                          !grepl("Control", expected_results$`Brief description`), ]
        
        if(nrow(t_expected_rows) >= i && nrow(p_expected_rows) >= i) {
          expected_t <- as.numeric(t_expected_rows$Result[i])
          expected_p <- as.numeric(p_expected_rows$Result[i])
          
          endpoint_detailed$Expected_T_Value[i] <- expected_t
          endpoint_detailed$Expected_P_Value[i] <- expected_p
          
          # Check matches with tolerance
          t_match <- abs(actual_t - expected_t) < 1e-6
          p_match <- abs(actual_p - expected_p) < 1e-4
          
          endpoint_detailed$T_Match[i] <- t_match
          endpoint_detailed$P_Match[i] <- p_match
          
          endpoint_total <- endpoint_total + 2  # T and P value
          if(t_match) endpoint_passed <- endpoint_passed + 1
          if(p_match) endpoint_passed <- endpoint_passed + 1
        }
      }
      
      detailed_results <- rbind(detailed_results, endpoint_detailed)
      total_comparisons <- total_comparisons + endpoint_total
      total_passed <- total_passed + endpoint_passed
      
      if(endpoint_passed < endpoint_total) {
        all_endpoints_passed <- FALSE
      }
      
      list(success = TRUE, comparisons = endpoint_total, passed = endpoint_passed)
      
    }, error = function(e) {
      cat("Error processing endpoint", endpoint, ":", e$message, "\n")
      
      # Add error row to detailed results
      error_row <- data.frame(
        Study_ID = study_id,
        Function_Group = function_group_id,
        Endpoint = endpoint,
        Comparison = "ERROR",
        Actual_T_Value = NA,
        Actual_P_Value = NA,
        Expected_T_Value = NA,
        Expected_P_Value = NA,
        T_Match = FALSE,
        P_Match = FALSE,
        stringsAsFactors = FALSE
      )
      detailed_results <<- rbind(detailed_results, error_row)
      all_endpoints_passed <<- FALSE
      
      list(success = FALSE, error = e$message)
    })
  }
  
  return(list(
    passed = all_endpoints_passed,
    endpoints_tested = endpoints,
    detailed_results = detailed_results,
    n_comparisons = total_comparisons,
    n_passed = total_passed
  ))
}