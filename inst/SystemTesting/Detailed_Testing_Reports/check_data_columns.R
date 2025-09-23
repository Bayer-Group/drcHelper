# Check the column names in test_cases_data
library(drcHelper)
data("test_cases_data")
data("test_cases_res")

cat("test_cases_data column names:\n")
print(names(test_cases_data))

cat("\ntest_cases_res column names:\n")
print(names(test_cases_res))

# Check if FG00225 exists in data
if("Function group ID" %in% names(test_cases_data)) {
  fg225_count <- sum(test_cases_data[["Function group ID"]] == "FG00225", na.rm = TRUE)
  cat("\nFG00225 rows in test_cases_data:", fg225_count, "\n")
}

# Check unique function group IDs in data
if("Function group ID" %in% names(test_cases_data)) {
  data_fg_ids <- unique(test_cases_data[["Function group ID"]])
  cat("\nFunction group IDs in data (first 10):\n")
  print(head(data_fg_ids, 10))
  
  cat("\nDoes FG00225 exist in data?", "FG00225" %in% data_fg_ids, "\n")
}