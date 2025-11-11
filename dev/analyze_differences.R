# Check the actual differences to determine appropriate tolerance
library(drcHelper)
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

# Test specific failing cases with detailed difference analysis
analyze_differences <- function(study_id, function_group_id, test_name) {
  cat("\n=== ANALYZING", test_name, "===\n")
  
  # Get expected results
  expected_results <- test_cases_res[
    test_cases_res[['Function group ID']] == function_group_id &
    test_cases_res[['Study ID']] == study_id &
    grepl("Dunnett", test_cases_res[['Brief description']]), ]
  
  test_endpoint <- unique(expected_results[['Endpoint']])[1]
  
  # Get and process study data
  study_data <- test_cases_data[
    test_cases_data[['Study ID']] == study_id & 
    test_cases_data[['Endpoint']] == test_endpoint, ]
  
  study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
  study_data <- study_data[!is.na(study_data$Dose_numeric), ]
  study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))
  
  test_data <- data.frame(
    Response = study_data$Response,
    Dose = study_data$Dose_numeric,
    Tank = study_data$Tank
  )
  
  control_level <- if (0 %in% test_data$Dose) 0 else min(test_data$Dose, na.rm = TRUE)
  
  # Run Dunnett test
  result <- dunnett_test(
    test_data,
    response_var = "Response",
    dose_var = "Dose", 
    tank_var = "Tank",
    control_level = control_level,
    include_random_effect = FALSE,
    alternative = "less"
  )
  
  expected_alt <- expected_results[grepl("smaller", expected_results[['Brief description']]), ]
  
  # Analyze t-value differences
  tvalue_expected <- expected_alt[grepl("t-value", expected_alt[['Brief description']]), ]
  results_df <- result$results_table
  
  cat("T-value comparisons:\n")
  differences <- c()
  
  for(i in 1:nrow(tvalue_expected)) {
    exp_dose <- convert_dose(tvalue_expected$Dose[i])
    exp_value <- as.numeric(tvalue_expected[['expected result value']][i])
    
    if(!is.na(exp_dose) && !is.na(exp_value)) {
      comparison_pattern <- paste0("^", exp_dose, " - ")
      result_row <- which(grepl(comparison_pattern, results_df$comparison))
      
      if(length(result_row) > 0) {
        actual_tstat <- results_df$statistic[result_row[1]]
        diff_val <- abs(actual_tstat - exp_value)
        differences <- c(differences, diff_val)
        
        cat(sprintf("  Dose %s: expected %.6f, actual %.6f, diff %.6f\n", 
                    exp_dose, exp_value, actual_tstat, diff_val))
      }
    }
  }
  
  # Analyze p-value differences
  pvalue_expected <- expected_alt[grepl("p-value", expected_alt[['Brief description']]), ]
  
  cat("P-value comparisons:\n")
  p_differences <- c()
  
  for(i in 1:min(5, nrow(pvalue_expected))) {
    exp_dose <- convert_dose(pvalue_expected$Dose[i])
    exp_pval <- as.numeric(pvalue_expected[['expected result value']][i])
    
    if(!is.na(exp_dose) && !is.na(exp_pval) && exp_dose != 0) {
      comparison_pattern <- paste0("^", exp_dose, " - ")
      result_row <- which(grepl(comparison_pattern, results_df$comparison))
      
      if(length(result_row) > 0) {
        actual_pval <- results_df$p.value[result_row[1]]
        diff_val <- abs(actual_pval - exp_pval)
        p_differences <- c(p_differences, diff_val)
        
        cat(sprintf("  Dose %s: expected %.6f, actual %.6f, diff %.6f\n", 
                    exp_dose, exp_pval, actual_pval, diff_val))
      }
    }
  }
  
  cat("Summary for", test_name, ":\n")
  if(length(differences) > 0) {
    cat(sprintf("  T-value diffs: min %.2e, max %.2e, median %.2e\n", 
                min(differences), max(differences), median(differences)))
  }
  if(length(p_differences) > 0) {
    cat(sprintf("  P-value diffs: min %.2e, max %.2e, median %.2e\n", 
                min(p_differences), max(p_differences), median(p_differences)))
  }
  
  return(list(t_diffs = differences, p_diffs = p_differences))
}

# Analyze all failing cases
cases <- list(
  list("MOCK08/15-001", "FG00221", "Aphidius Reproduction"),
  list("MOCK08/15-001", "FG00222", "Aphidius Repellency"), 
  list("MOCKSE21/001-1", "FG00225", "BRSOL Plant Tests")
)

all_t_diffs <- c()
all_p_diffs <- c()

for(case in cases) {
  result <- analyze_differences(case[[1]], case[[2]], case[[3]])
  all_t_diffs <- c(all_t_diffs, result$t_diffs)
  all_p_diffs <- c(all_p_diffs, result$p_diffs)
}

cat("\n=== OVERALL ANALYSIS ===\n")
cat("Current tolerance settings:\n")
cat("  T-value tolerance: 1e-6\n")
cat("  P-value tolerance: 1e-4\n\n")

if(length(all_t_diffs) > 0) {
  cat("All T-value differences:\n")
  cat(sprintf("  Range: %.2e to %.2e\n", min(all_t_diffs), max(all_t_diffs)))
  cat(sprintf("  Median: %.2e\n", median(all_t_diffs)))
  cat(sprintf("  95th percentile: %.2e\n", quantile(all_t_diffs, 0.95)))
  
  suggested_t_tol <- max(all_t_diffs) * 2  # 2x the maximum difference
  cat(sprintf("  Suggested tolerance: %.2e\n", suggested_t_tol))
}

if(length(all_p_diffs) > 0) {
  cat("\nAll P-value differences:\n")
  cat(sprintf("  Range: %.2e to %.2e\n", min(all_p_diffs), max(all_p_diffs)))
  cat(sprintf("  Median: %.2e\n", median(all_p_diffs)))
  cat(sprintf("  95th percentile: %.2e\n", quantile(all_p_diffs, 0.95)))
  
  suggested_p_tol <- max(all_p_diffs) * 2  # 2x the maximum difference
  cat(sprintf("  Suggested tolerance: %.2e\n", suggested_p_tol))
}