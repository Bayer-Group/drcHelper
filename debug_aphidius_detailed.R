# Debug Aphidius Reproduction detailed validation
library(drcHelper)
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

tolerance <- 1e-6
p_value_tolerance <- 1e-4

convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

# Test FG00221 (Aphidius Reproduction)
expected_results <- test_cases_res[
  test_cases_res[['Function group ID']] == "FG00221" &
  test_cases_res[['Study ID']] == "MOCK08/15-001" &
  grepl("Dunnett", test_cases_res[['Brief description']]), ]

test_endpoint <- unique(expected_results[['Endpoint']])[1]
cat("Test endpoint:", test_endpoint, "\n")

study_data <- test_cases_data[
  test_cases_data[['Study ID']] == "MOCK08/15-001" & 
  test_cases_data[['Endpoint']] == test_endpoint, ]

# Process data
study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
study_data <- study_data[!is.na(study_data$Dose_numeric), ]
study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))

test_data <- data.frame(
  Response = study_data$Response,
  Dose = study_data$Dose_numeric,
  Tank = study_data$Tank
)

cat("Doses:", paste(unique(test_data$Dose), collapse=", "), "\n")

# Run Dunnett test
result <- dunnett_test(
  test_data,
  response_var = "Response",
  dose_var = "Dose", 
  tank_var = "Tank",
  control_level = 0,
  include_random_effect = FALSE,
  alternative = "less"
)

cat("Dunnett results:\n")
print(result$results_table)

# Check expected results for validation
expected_alt <- expected_results[grepl("smaller", expected_results[['Brief description']]), ]
tvalue_expected <- expected_alt[grepl("t-value", expected_alt[['Brief description']]), ]
cat("\nExpected t-values found:", nrow(tvalue_expected), "\n")

# Create detailed validation table like BRSOL
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
  
  for(i in 1:nrow(tvalue_expected)) {
    exp_dose <- convert_dose(tvalue_expected$Dose[i])
    exp_value <- as.numeric(tvalue_expected[['expected result value']][i])
    
    if(!is.na(exp_dose) && !is.na(exp_value)) {
      comparison_pattern <- paste0("^", exp_dose, " - ")
      result_row <- which(grepl(comparison_pattern, results_df$comparison))
      
      if(length(result_row) > 0) {
        actual_tstat <- results_df$statistic[result_row[1]]
        diff_val <- abs(actual_tstat - exp_value)
        passed <- diff_val < tolerance
        
        validation_results <- rbind(validation_results, data.frame(
          metric = paste("T-statistic at dose", exp_dose),
          expected = exp_value,
          actual = actual_tstat,
          diff = diff_val,
          passed = passed
        ))
      }
    }
  }
}

# Check p-values too
pvalue_expected <- expected_alt[grepl("p-value", expected_alt[['Brief description']]), ]
cat("Expected p-values found:", nrow(pvalue_expected), "\n")

if(nrow(pvalue_expected) > 0) {
  results_df <- result$results_table
  
  for(i in 1:nrow(pvalue_expected)) {
    exp_dose <- convert_dose(pvalue_expected$Dose[i])
    exp_pval <- as.numeric(pvalue_expected[['expected result value']][i])
    
    if(!is.na(exp_dose) && !is.na(exp_pval) && exp_dose != 0) {  # Skip control
      comparison_pattern <- paste0("^", exp_dose, " - ")
      result_row <- which(grepl(comparison_pattern, results_df$comparison))
      
      if(length(result_row) > 0) {
        actual_pval <- results_df$p.value[result_row[1]]
        diff_val <- abs(actual_pval - exp_pval)
        passed <- diff_val < p_value_tolerance
        
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

cat("\n=== DETAILED VALIDATION TABLE ===\n")
cat("Total validations:", nrow(validation_results), "\n")
cat("Passed validations:", sum(validation_results$passed), "\n")
cat("Failed validations:", sum(!validation_results$passed), "\n")

if(nrow(validation_results) > 0) {
  cat("\nDetailed comparison table:\n")
  # Format for display like the BRSOL case
  display_table <- validation_results
  display_table$Tolerance <- ifelse(grepl("P-value", display_table$metric), p_value_tolerance, tolerance)
  display_table$Status <- ifelse(display_table$passed, "PASS", "FAIL")
  
  print(display_table[, c("metric", "expected", "actual", "diff", "Tolerance", "Status")])
}

# Show which ones are failing and why
failures <- validation_results[!validation_results$passed, ]
if(nrow(failures) > 0) {
  cat("\nFAILED VALIDATIONS:\n")
  for(i in 1:nrow(failures)) {
    cat(sprintf("%s: expected %f, actual %f, diff %f (tolerance %f)\n", 
                failures$metric[i], failures$expected[i], failures$actual[i], 
                failures$diff[i], tolerance))
  }
}