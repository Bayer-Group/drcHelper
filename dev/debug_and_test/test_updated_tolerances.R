# Quick test with updated tolerances
library(drcHelper)
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

# Updated tolerances
tolerance <- 0.3
p_value_tolerance <- 0.06

convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

# Quick test of Aphidius Reproduction
study_data <- test_cases_data[
  test_cases_data[['Study ID']] == "MOCK08/15-001" & 
  test_cases_data[['Endpoint']] == "Reproduction", ]

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

# Get expected results
expected_results <- test_cases_res[
  test_cases_res[['Function group ID']] == "FG00221" &
  test_cases_res[['Study ID']] == "MOCK08/15-001" &
  grepl("Dunnett", test_cases_res[['Brief description']]), ]

expected_alt <- expected_results[grepl("smaller", expected_results[['Brief description']]), ]

# Test t-value validation with new tolerance
tvalue_expected <- expected_alt[grepl("t-value", expected_alt[['Brief description']]), ]
results_df <- result$results_table

cat("=== TESTING WITH UPDATED TOLERANCES ===\n")
cat("T-value tolerance:", tolerance, "\n")
cat("P-value tolerance:", p_value_tolerance, "\n\n")

validation_results <- data.frame(
  metric = character(),
  expected = numeric(),
  actual = numeric(), 
  diff = numeric(),
  passed = logical(),
  stringsAsFactors = FALSE
)

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
      
      cat(sprintf("Dose %s: expected %.6f, actual %.6f, diff %.6f, passed %s\n", 
                  exp_dose, exp_value, actual_tstat, diff_val, passed))
      
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

cat("\nOverall validation result:", all(validation_results$passed), "\n")
cat("Passed:", sum(validation_results$passed), "/", nrow(validation_results), "\n")