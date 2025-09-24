# Debug why the tests are now failing
library(drcHelper)
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

tolerance <- 1e-6
p_value_tolerance <- 1e-4

convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

# Test each failing case to see the specific errors
test_cases <- list(
  list(study = "MOCK08/15-001", fg = "FG00221", name = "Aphidius Reproduction"),
  list(study = "MOCK08/15-001", fg = "FG00222", name = "Aphidius Repellency"), 
  list(study = "MOCKSE21/001-1", fg = "FG00225", name = "BRSOL Plant Tests")
)

for(case in test_cases) {
  cat("\n=== DEBUGGING", case$name, "===\n")
  
  # Get expected results
  expected_results <- test_cases_res[
    test_cases_res[['Function group ID']] == case$fg &
    test_cases_res[['Study ID']] == case$study &
    grepl("Dunnett", test_cases_res[['Brief description']]), ]
  
  cat("Expected results found:", nrow(expected_results), "\n")
  
  if(nrow(expected_results) > 0) {
    test_endpoint <- unique(expected_results[['Endpoint']])[1]
    cat("Test endpoint:", test_endpoint, "\n")
    
    # Get study data
    study_data <- test_cases_data[
      test_cases_data[['Study ID']] == case$study & 
      test_cases_data[['Endpoint']] == test_endpoint, ]
    
    cat("Study data rows:", nrow(study_data), "\n")
    
    if(nrow(study_data) > 0) {
      # Convert doses
      study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
      study_data <- study_data[!is.na(study_data$Dose_numeric), ]
      
      cat("After dose conversion:", nrow(study_data), "\n")
      cat("Dose range:", min(study_data$Dose_numeric), "to", max(study_data$Dose_numeric), "\n")
      
      # Check expected results for 'smaller' alternative
      expected_alt <- expected_results[grepl("smaller", expected_results[['Brief description']]), ]
      cat("Expected 'smaller' results:", nrow(expected_alt), "\n")
      
      # Check count data
      has_count_data <- any(!is.na(study_data$Total)) || 
                        any(!is.na(study_data$Alive)) || 
                        any(!is.na(study_data$Dead))
      cat("Has count data:", has_count_data, "\n")
      
      if(!has_count_data) {
        # Try to run Dunnett test
        study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))
        
        test_data <- data.frame(
          Response = study_data$Response,
          Dose = study_data$Dose_numeric,
          Tank = study_data$Tank
        )
        
        control_level <- if (0 %in% test_data$Dose) {
          0
        } else {
          min(test_data$Dose, na.rm = TRUE)
        }
        
        cat("Control level:", control_level, "\n")
        
        tryCatch({
          result <- dunnett_test(
            test_data,
            response_var = "Response",
            dose_var = "Dose", 
            tank_var = "Tank",
            control_level = control_level,
            include_random_effect = FALSE,
            alternative = "less"
          )
          
          cat("Dunnett test successful, results:", nrow(result$results_table), "rows\n")
          
          # Check for t-value matches
          tvalue_expected <- expected_alt[grepl("t-value", expected_alt[['Brief description']]), ]
          cat("T-value expected entries:", nrow(tvalue_expected), "\n")
          
          if(nrow(tvalue_expected) > 0 && !is.null(result$results_table)) {
            cat("First few expected t-values:\n")
            for(i in 1:min(3, nrow(tvalue_expected))) {
              dose <- convert_dose(tvalue_expected$Dose[i])
              exp_val <- as.numeric(tvalue_expected[['expected result value']][i])
              cat(sprintf("  Dose %s: expected %f\n", dose, exp_val))
            }
            
            cat("Actual results table:\n")
            print(result$results_table[1:min(3, nrow(result$results_table)), c("comparison", "statistic", "p.value")])
          }
          
        }, error = function(e) {
          cat("ERROR in dunnett_test:", e$message, "\n")
        })
      } else {
        cat("Skipping - has count data\n")
      }
    }
  }
}