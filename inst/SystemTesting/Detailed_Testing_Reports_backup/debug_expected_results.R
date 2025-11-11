# Debug expected results for FG00225
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

cat("Checking expected results for FG00225...\n")

# Check what's in the expected results
fg225_expected <- test_cases_res[
  test_cases_res[['Study ID']] == "2019-IVA-001" &
  test_cases_res[['Function group ID']] == "FG00225", ]

cat("Total expected results for FG00225:", nrow(fg225_expected), "\n")

if(nrow(fg225_expected) > 0) {
  cat("\nBrief descriptions:\n")
  print(unique(fg225_expected[['Brief description']]))
  
  cat("\nDunnett related results:\n")
  dunnett_results <- fg225_expected[grepl("Dunnett", fg225_expected[['Brief description']]), ]
  cat("Count:", nrow(dunnett_results), "\n")
  if(nrow(dunnett_results) > 0) {
    print(dunnett_results[, c("Brief description", "Endpoint", "expected result value")])
  }
  
  # Check available endpoints
  cat("\nAvailable endpoints:\n")
  print(unique(fg225_expected[['Endpoint']]))
  
  # Check the specific pattern matching
  cat("\nChecking for 'smaller' pattern:\n")
  smaller_results <- fg225_expected[grepl("smaller", fg225_expected[['Brief description']]), ]
  cat("Count:", nrow(smaller_results), "\n")
  if(nrow(smaller_results) > 0) {
    print(smaller_results[, c("Brief description", "Endpoint")])
  }
} else {
  cat("No expected results found for FG00225\n")
  
  # Check what function groups are available
  cat("\nAvailable function groups:\n")
  print(unique(test_cases_res[['Function group ID']]))
  
  # Check what study IDs are available 
  cat("\nAvailable study IDs:\n")
  print(unique(test_cases_res[['Study ID']]))
}