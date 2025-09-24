# Test the fixed validation function with correct patterns
library(drcHelper)
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

tolerance <- 1e-6
p_value_tolerance <- 1e-4

convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

# Get FG00225 data and run dunnett test
study_data <- test_cases_data[
  test_cases_data[['Study ID']] == "MOCKSE21/001-1" & 
  test_cases_data[['Endpoint']] == "Plant height", ]

study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
study_data <- study_data[!is.na(study_data$Dose_numeric), ]
study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))

test_data <- data.frame(
  Response = study_data$Response,
  Dose = study_data$Dose_numeric,
  Tank = study_data$Tank
)

result <- dunnett_test(
  test_data,
  response_var = "Response",
  dose_var = "Dose", 
  tank_var = "Tank",
  control_level = 0,
  include_random_effect = FALSE,
  alternative = "less"
)

# Get expected results with correct patterns
expected_results <- test_cases_res[
  test_cases_res[['Function group ID']] == "FG00225" &
  test_cases_res[['Study ID']] == "MOCKSE21/001-1" &
  grepl("Dunnett", test_cases_res[['Brief description']]), ]

expected_alt <- expected_results[grepl("smaller", expected_results[['Brief description']]), ]

cat("=== TESTING WITH CORRECTED PATTERNS ===\n")

# Test t-value comparisons with correct pattern
tvalue_expected <- expected_alt[grepl("t-value", expected_alt[['Brief description']]), ]
cat("T-value expected results:", nrow(tvalue_expected), "\n")

validation_results <- data.frame(
  metric = character(),
  expected = numeric(),
  actual = numeric(), 
  diff = numeric(),
  passed = logical(),
  stringsAsFactors = FALSE
)

if(nrow(tvalue_expected) > 0) {
  results_df <- result$results_table
  
  for(i in 1:min(5, nrow(tvalue_expected))) {
    exp_dose <- convert_dose(tvalue_expected$Dose[i])
    exp_value <- as.numeric(tvalue_expected[['expected result value']][i])
    
    cat(sprintf("Checking T-value at dose %s: expected %f\n", exp_dose, exp_value))
    
    # Find corresponding t-statistic in results
    comparison_pattern <- paste0("^", exp_dose, " - ")
    result_row <- which(grepl(comparison_pattern, results_df$comparison))
    
    if(length(result_row) > 0) {
      actual_tstat <- results_df$statistic[result_row[1]]
      diff_val <- abs(actual_tstat - exp_value)
      passed <- diff_val < tolerance
      
      cat(sprintf("  Found: actual %f, diff %f, passed %s\n", actual_tstat, diff_val, passed))
      
      validation_results <- rbind(validation_results, data.frame(
        metric = paste("T-statistic at dose", exp_dose),
        expected = exp_value,
        actual = actual_tstat,
        diff = diff_val,
        passed = passed
      ))
    } else {
      cat(sprintf("  No match for pattern '%s'\n", comparison_pattern))
    }
  }
}

# Test p-value comparisons
pvalue_expected <- expected_alt[grepl("p-value", expected_alt[['Brief description']]), ]
cat("\nP-value expected results:", nrow(pvalue_expected), "\n")

if(nrow(pvalue_expected) > 0) {
  results_df <- result$results_table
  
  for(i in 1:min(3, nrow(pvalue_expected))) {
    exp_dose <- convert_dose(pvalue_expected$Dose[i])
    exp_pval <- as.numeric(pvalue_expected[['expected result value']][i])
    
    if(!is.na(exp_dose) && exp_dose != 0) {  # Skip control comparisons for now
      cat(sprintf("Checking P-value at dose %s: expected %f\n", exp_dose, exp_pval))
      
      comparison_pattern <- paste0("^", exp_dose, " - ")
      result_row <- which(grepl(comparison_pattern, results_df$comparison))
      
      if(length(result_row) > 0) {
        actual_pval <- results_df$p.value[result_row[1]]
        diff_val <- abs(actual_pval - exp_pval)
        passed <- diff_val < p_value_tolerance
        
        cat(sprintf("  Found: actual %f, diff %f, passed %s\n", actual_pval, diff_val, passed))
        
        validation_results <- rbind(validation_results, data.frame(
          metric = paste("P-value at dose", exp_dose),
          expected = exp_pval,
          actual = actual_pval,
          diff = diff_val,
          passed = passed
        ))
      }
    }
  }
}

cat("\n=== VALIDATION SUMMARY ===\n")
cat("Total validations:", nrow(validation_results), "\n")
cat("Passed validations:", sum(validation_results$passed), "\n")
cat("Overall success:", all(validation_results$passed), "\n")

if(nrow(validation_results) > 0) {
  cat("\nDetailed results:\n")
  print(validation_results)
}