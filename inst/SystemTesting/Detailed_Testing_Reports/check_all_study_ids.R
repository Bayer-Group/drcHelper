# Check correct Study IDs for all function groups
library(drcHelper)
data("test_cases_res")

# Check each function group
function_groups <- c("FG00220", "FG00221", "FG00222", "FG00223", "FG00224", "FG00225")

cat("Function Group Study ID mappings:\n")
for(fg in function_groups) {
  fg_data <- test_cases_res[test_cases_res[['Function group ID']] == fg, ]
  if(nrow(fg_data) > 0) {
    study_id <- unique(fg_data[['Study ID']])
    cat(sprintf("%-7s -> %s\n", fg, study_id))
  } else {
    cat(sprintf("%-7s -> No data found\n", fg))
  }
}