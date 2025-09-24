# Test the actual dunnett_test function call to see what's failing
library(drcHelper)

load('data/test_cases_data.rda')

# Get BRSOL Plant height data
study_data <- test_cases_data[
  test_cases_data[['Study ID']] == "MOCKSE21/001-1" & 
  test_cases_data[['Endpoint']] == "Plant height", ]

cat("Study data rows:", nrow(study_data), "\n")
cat("First few Response values:", paste(head(study_data$Response), collapse=", "), "\n")
cat("First few Dose values:", paste(head(study_data$Dose), collapse=", "), "\n")

# Convert doses
convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "n/a") return(NA)
  as.numeric(gsub(",", ".", dose_str))
}

study_data$Dose_numeric <- sapply(study_data$Dose, convert_dose)
study_data <- study_data[!is.na(study_data$Dose_numeric), ]

cat("After dose conversion:", nrow(study_data), "\n")
cat("Dose range:", min(study_data$Dose_numeric), "to", max(study_data$Dose_numeric), "\n")

# Create Tank variable and test data
study_data$Tank <- rep(1:max(table(study_data$Dose_numeric)), length.out = nrow(study_data))

test_data <- data.frame(
  Response = study_data$Response,
  Dose = study_data$Dose_numeric,
  Tank = study_data$Tank
)

cat("Test data structure:\n")
str(test_data)
cat("Tank distribution:\n")
print(table(test_data$Tank, test_data$Dose))

cat("\nAttempting dunnett_test call...\n")

tryCatch({
  result <- dunnett_test(
    test_data,
    response_var = "Response",
    dose_var = "Dose", 
    tank_var = "Tank",
    control_level = 0,
    include_random_effect = FALSE,
    alternative = "less"
  )
  
  cat("SUCCESS! Dunnett test completed\n")
  cat("Result structure:\n")
  cat("- results_table rows:", ifelse(is.null(result$results_table), "NULL", nrow(result$results_table)), "\n")
  cat("- noec:", result$noec, "\n")
  cat("- model_type:", result$model_type, "\n")
  
  if(!is.null(result$results_table)) {
    cat("Sample results:\n")
    print(head(result$results_table, 3))
  }
  
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  cat("Full error:\n")
  print(e)
})