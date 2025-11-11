# Validation test - apply the matching logic to sample data
# This simulates what the actual testing functions should do

load("data/test_cases_data.rda")
load("data/test_cases_res.rda")

# Sample test function that uses correct matching logic
test_data_matching <- function() {
  cat("=== VALIDATION TEST ===\n")
  
  # Get unique combinations to test
  unique_cases <- unique(test_cases_data[c("Study ID", "Endpoint", "Measurement Variable")])
  
  for (i in 1:nrow(unique_cases)) {
    case <- unique_cases[i, ]
    study_id <- case$`Study ID`
    endpoint <- case$Endpoint
    measurement_var <- case$`Measurement Variable`
    
    # Apply the correct matching logic
    if (study_id == "MOCK0065") {
      # Myriophyllum: match all three fields
      matches <- test_cases_res[
        test_cases_res$`Study ID` == study_id &
        test_cases_res$Endpoint == endpoint &
        test_cases_res$`Measurement \r\nvaribale` == measurement_var,
      ]
    } else {
      # Other studies: match only Study ID + Endpoint
      matches <- test_cases_res[
        test_cases_res$`Study ID` == study_id &
        test_cases_res$Endpoint == endpoint,
      ]
    }
    
    cat(sprintf("Study: %-15s Endpoint: %-25s Matches: %d\n", 
                study_id, endpoint, nrow(matches)))
    
    if (nrow(matches) == 0) {
      cat("   *** WARNING: No matches found! ***\n")
    }
  }
}

# Run the validation test
test_data_matching()

cat("\n=== SUMMARY ===\n")
cat("The matching logic should be:\n")
cat("- MOCK0065 (Myriophyllum): Match Study ID + Endpoint + Measurement Variable\n")
cat("- All other studies: Match Study ID + Endpoint only\n")
cat("\nThis accounts for the fact that non-Myriophyllum studies have 'n/a'\n")
cat("in the data but specific measurement variables in the results.\n")