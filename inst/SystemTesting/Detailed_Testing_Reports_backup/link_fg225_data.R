# Check how to link data and expected results for FG00225
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

cat("Investigating how to link FG00225 data and expected results...\n")

# Get FG00225 expected results
fg225_expected <- test_cases_res[test_cases_res[['Function group ID']] == "FG00225", ]
cat("FG00225 expected results count:", nrow(fg225_expected), "\n")

if(nrow(fg225_expected) > 0) {
  # Check study ID and endpoints
  study_id <- unique(fg225_expected[['Study ID']])
  endpoints <- unique(fg225_expected[['Endpoint']])
  
  cat("Study ID:", study_id, "\n")
  cat("Endpoints:", paste(endpoints, collapse = ", "), "\n")
  
  # Now find matching data based on Study ID and Endpoints
  matching_data <- test_cases_data[
    test_cases_data[['Study ID']] == study_id &
    test_cases_data[['Endpoint']] %in% endpoints, ]
  
  cat("\nMatching data rows:", nrow(matching_data), "\n")
  
  if(nrow(matching_data) > 0) {
    cat("Data endpoints found:", paste(unique(matching_data[['Endpoint']]), collapse = ", "), "\n")
    cat("Data dose levels:", paste(sort(unique(matching_data[['Dose']])), collapse = ", "), "\n")
    
    # Sample of the data
    cat("\nFirst few rows of matching data:\n")
    print(head(matching_data[, c("Study ID", "Endpoint", "Dose", "Response")]))
  }
}