# Investigation of Repellency Data Alignment Issues
# Checking for misalignment and missing endpoints

library(drcHelper)
library(drc)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

# Load the required datasets
data("test_cases_data")
data("test_cases_res")

# Focus on Repellency study (FG00225)
repellency_study_id <- "MOCKSE21/001-1"
repellency_fg_id <- "FG00225"

cat("=== REPELLENCY DATA INVESTIGATION ===\n\n")

# 1. Extract raw data for Repellency study
cat("1. RAW STUDY DATA:\n")
repellency_data <- test_cases_data[test_cases_data[['Study ID']] == repellency_study_id, ]
cat("Total rows in", repellency_study_id, "study:", nrow(repellency_data), "\n")
cat("Unique endpoints:", paste(unique(repellency_data$Endpoint), collapse=", "), "\n")
cat("Unique doses:", paste(sort(unique(repellency_data$Dose)), collapse=", "), "\n\n")

# Check endpoint distribution
endpoint_counts <- table(repellency_data$Endpoint)
print("Endpoint distribution:")
print(endpoint_counts)
cat("\n")

# 2. Calculate means for each dose-endpoint combination
cat("2. CALCULATED MEANS BY DOSE AND ENDPOINT:\n")
means_data <- repellency_data %>%
  group_by(Dose, Endpoint) %>%
  summarise(
    n_obs = n(),
    mean_value = mean(Response, na.rm = TRUE),
    sd_value = sd(Response, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Endpoint, Dose)

print(kable(means_data, digits = 4, caption = "Calculated Means by Dose and Endpoint"))
cat("\n")

# 3. Extract expected results for Repellency
cat("3. EXPECTED RESULTS FROM test_cases_res:\n")
expected_results <- test_cases_res[
  test_cases_res[['Study ID']] == repellency_study_id &
  test_cases_res[['Function group ID']] == repellency_fg_id, ]

cat("Total expected results:", nrow(expected_results), "\n")
cat("Unique test methods:", paste(unique(expected_results[['Test method']]), collapse=", "), "\n")
cat("Unique endpoints in expected:", paste(unique(expected_results[['Endpoint']]), collapse=", "), "\n\n")

# Show expected results structure
expected_sample <- expected_results[1:10, c("Brief description", "Endpoint", "Test group", "Dose", "expected result value")]
print("Sample of expected results:")
print(kable(expected_sample, caption = "Sample Expected Results"))
cat("\n")

# 4. Check for Plant Height vs Shoot Dry Weight
cat("4. ENDPOINT ANALYSIS:\n")
plant_height_data <- repellency_data[repellency_data$Endpoint == "Plant height", ]
shoot_weight_data <- repellency_data[repellency_data$Endpoint == "Shoot dry weight", ]

cat("Plant height observations:", nrow(plant_height_data), "\n")
cat("Shoot dry weight observations:", nrow(shoot_weight_data), "\n")

if(nrow(shoot_weight_data) > 0) {
  cat("Shoot dry weight doses:", paste(sort(unique(shoot_weight_data$Dose)), collapse=", "), "\n")
  shoot_means <- shoot_weight_data %>%
    group_by(Dose) %>%
    summarise(mean_weight = mean(Response, na.rm = TRUE), .groups = 'drop')
  print("Shoot dry weight means:")
  print(shoot_means)
} else {
  cat("No shoot dry weight data found in raw data!\n")
}
cat("\n")

# 5. Run Dunnett test on Plant Height data
cat("5. DUNNETT TEST ON PLANT HEIGHT:\n")
plant_height_subset <- plant_height_data[, c("Dose", "Response")]
plant_height_subset$Dose <- as.factor(plant_height_subset$Dose)

# Run Dunnett test
dunnett_result <- try(dunnett_test(plant_height_subset$Response ~ plant_height_subset$Dose), silent = TRUE)
if(class(dunnett_result) == "try-error") {
  # Try alternative function call
  dunnett_result <- try(broom_dunnett(plant_height_subset, "Dose", "Response"), silent = TRUE)
}
cat("Dunnett test results:\n")
if(class(dunnett_result) != "try-error") {
  print(dunnett_result)
} else {
  cat("Error running Dunnett test - will use manual comparison\n")
  # Calculate manual group means
  group_means <- plant_height_subset %>%
    group_by(Dose) %>%
    summarise(mean_response = mean(Response, na.rm = TRUE), 
              n = n(), 
              sd = sd(Response, na.rm = TRUE),
              .groups = 'drop')
  print(group_means)
}
cat("\n")

# 6. Check alignment by testing row shifts
cat("6. TESTING ROW SHIFT HYPOTHESIS:\n")
# Get expected means for comparison
expected_means <- expected_results[grepl("mean", expected_results[['Brief description']]), ]
cat("Expected means found:", nrow(expected_means), "\n")

if(nrow(expected_means) > 0) {
  # Extract expected mean values and doses
  expected_means_clean <- expected_means[, c("Dose", "expected result value", "Test group", "Endpoint")]
  expected_means_clean$expected_numeric <- as.numeric(expected_means_clean[['expected result value']])
  
  print("Expected means:")
  print(kable(expected_means_clean, digits = 4))
  
  # Test different row shifts
  cat("\nTesting alignment with calculated means:\n")
  calculated_means <- means_data[means_data$Endpoint == "Plant height", ]
  
  for(shift in -2:2) {
    if(shift == 0) {
      cat("No shift (original alignment):\n")
    } else {
      cat("Shift by", shift, "rows:\n")
    }
    
    # Apply shift to expected results
    shifted_idx <- seq_len(nrow(expected_means_clean)) + shift
    valid_idx <- shifted_idx > 0 & shifted_idx <= nrow(expected_means_clean)
    
    if(any(valid_idx)) {
      for(i in which(valid_idx)) {
        exp_idx <- shifted_idx[i]
        if(exp_idx <= nrow(expected_means_clean) && i <= nrow(calculated_means)) {
          exp_dose <- expected_means_clean$Dose[exp_idx]
          exp_val <- expected_means_clean$expected_numeric[exp_idx]
          calc_row <- calculated_means[calculated_means$Dose == exp_dose, ]
          
          if(nrow(calc_row) > 0) {
            diff_val <- abs(calc_row$mean_value - exp_val)
            cat(sprintf("  Dose %s: Expected=%.4f, Calculated=%.4f, Diff=%.6f\n", 
                       exp_dose, exp_val, calc_row$mean_value, diff_val))
          }
        }
      }
    }
    cat("\n")
  }
}

# 7. Plot the data
cat("7. CREATING PLOTS:\n")
# Plot Plant Height data
p1 <- ggplot(plant_height_data, aes(x = factor(Dose), y = Response)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(title = paste("Plant Height Data -", repellency_study_id, "Study"),
       x = "Dose", y = "Plant Height Response") +
  theme_minimal()

print(p1)

# If shoot dry weight data exists, plot it too
if(nrow(shoot_weight_data) > 0) {
  p2 <- ggplot(shoot_weight_data, aes(x = factor(Dose), y = Response)) +
    geom_boxplot(alpha = 0.7) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
    labs(title = paste("Shoot Dry Weight Data -", repellency_study_id, "Study"),
         x = "Dose", y = "Shoot Dry Weight Response") +
    theme_minimal()
  
  print(p2)
}

# 8. Summary
cat("8. SUMMARY OF FINDINGS:\n")
cat("- Plant height observations:", nrow(plant_height_data), "\n")
cat("- Shoot dry weight observations:", nrow(shoot_weight_data), "\n")
cat("- Expected results entries:", nrow(expected_results), "\n")
cat("- Expected means entries:", nrow(expected_means), "\n")
cat("- Dunnett test comparisons:", nrow(dunnett_result), "\n")

cat("\nInvestigation complete. Check the alignment analysis above for potential row shift solutions.\n")