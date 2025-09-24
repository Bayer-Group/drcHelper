# Test script to verify data matching logic
# The issue: measurement variable matching should be different for Myriophyllum vs other studies

# Load test data
load("data/test_cases_data.rda")
load("data/test_cases_res.rda")

# Check the data structure
cat("=== TEST_CASES_DATA STRUCTURE ===\n")
cat("Unique Study ID + Endpoint + Measurement Variable combinations:\n")
data_combinations <- unique(test_cases_data[c("Study ID", "Endpoint", "Measurement Variable")])
print(data_combinations)

cat("\n=== TEST_CASES_RES STRUCTURE ===\n") 
cat("Unique Study ID + Endpoint + Measurement Variable combinations:\n")
# Note: column name has special characters
res_combinations <- unique(test_cases_res[c("Study ID", "Endpoint", "Measurement \r\nvaribale")])
names(res_combinations)[3] <- "Measurement Variable"  # Rename for easier handling
print(res_combinations)

cat("\n=== MATCHING LOGIC TEST ===\n")

# Test 1: Myriophyllum case (MOCK0065) - should match on all three fields
cat("1. Myriophyllum case (MOCK0065):\n")
myrio_data <- test_cases_data[test_cases_data$`Study ID` == "MOCK0065", ]
myrio_res <- test_cases_res[test_cases_res$`Study ID` == "MOCK0065", ]

cat("   Data measurement variable:", unique(myrio_data$`Measurement Variable`), "\n")
cat("   Results measurement variable:", unique(myrio_res$`Measurement \r\nvaribale`), "\n")
cat("   Should match exactly: TRUE\n")

# Test 2: Other studies - should match only on Study ID + Endpoint
cat("\n2. Other studies (e.g., MOCK08/15-001):\n")
other_data <- test_cases_data[test_cases_data$`Study ID` == "MOCK08/15-001", ]
other_res <- test_cases_res[test_cases_res$`Study ID` == "MOCK08/15-001", ]

cat("   Data measurement variable:", unique(other_data$`Measurement Variable`), "\n")
cat("   Results measurement variable:", unique(other_res$`Measurement \r\nvaribale`), "\n")
cat("   Should match only on Study ID + Endpoint, ignore measurement variable\n")

cat("\n=== PROPOSED MATCHING FUNCTION ===\n")

# Function to match data and results based on the correct logic
match_test_data <- function(data_df, res_df) {
  results <- list()
  
  for (i in 1:nrow(data_df)) {
    study_id <- data_df$`Study ID`[i]
    endpoint <- data_df$Endpoint[i]
    measurement_var <- data_df$`Measurement Variable`[i]
    
    if (study_id == "MOCK0065") {
      # Myriophyllum: match all three fields
      matches <- res_df[
        res_df$`Study ID` == study_id &
        res_df$Endpoint == endpoint &
        res_df$`Measurement \r\nvaribale` == measurement_var,
      ]
    } else {
      # Other studies: match only Study ID + Endpoint
      matches <- res_df[
        res_df$`Study ID` == study_id &
        res_df$Endpoint == endpoint,
      ]
    }
    
    results[[i]] <- list(
      study_id = study_id,
      endpoint = endpoint,
      measurement_var = measurement_var,
      matches_found = nrow(matches)
    )
  }
  
  return(results)
}

# Test the matching function on a sample
cat("Testing matching function on first few combinations:\n")
sample_data <- data_combinations[1:5, ]
test_results <- match_test_data(sample_data, test_cases_res)

for (i in 1:length(test_results)) {
  result <- test_results[[i]]
  cat(sprintf("Study: %s, Endpoint: %s, Matches: %d\n", 
              result$study_id, result$endpoint, result$matches_found))
}