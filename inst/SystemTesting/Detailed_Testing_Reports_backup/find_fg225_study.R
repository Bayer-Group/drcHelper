# Find the correct study ID for FG00225
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

cat("Finding study ID for FG00225...\n")

# Check what study ID FG00225 is associated with in the expected results
fg225_all <- test_cases_res[test_cases_res[['Function group ID']] == "FG00225", ]
cat("FG00225 expected results count:", nrow(fg225_all), "\n")

if(nrow(fg225_all) > 0) {
  cat("Study ID for FG00225:", unique(fg225_all[['Study ID']]), "\n")
  cat("Available endpoints:", paste(unique(fg225_all[['Endpoint']]), collapse = ", "), "\n")
  
  # Check for Dunnett results
  dunnett_results <- fg225_all[grepl("Dunnett", fg225_all[['Brief description']]), ]
  cat("Dunnett results count:", nrow(dunnett_results), "\n")
  
  if(nrow(dunnett_results) > 0) {
    cat("\nDunnett descriptions:\n")
    print(unique(dunnett_results[['Brief description']]))
  }
}

# Also check the data side
cat("\n--- Checking test data ---\n")
fg225_data <- test_cases_data[test_cases_data[['Function group ID']] == "FG00225", ]
cat("FG00225 data count:", nrow(fg225_data), "\n")

if(nrow(fg225_data) > 0) {
  cat("Study ID in data:", unique(fg225_data[['Study ID']]), "\n")
  cat("Endpoints in data:", paste(unique(fg225_data[['Endpoint']]), collapse = ", "), "\n")
}