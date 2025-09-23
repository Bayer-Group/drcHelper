# Detailed Repellency Data Alignment Analysis
# Looking specifically at the dose misalignment issue

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

cat("=== DETAILED ALIGNMENT ANALYSIS ===\n\n")

# Get the data
repellency_data <- test_cases_data[test_cases_data[['Study ID']] == repellency_study_id, ]
expected_results <- test_cases_res[
  test_cases_res[['Study ID']] == repellency_study_id &
  test_cases_res[['Function group ID']] == repellency_fg_id, ]

# Focus on means only for cleaner analysis
expected_means <- expected_results[grepl("Mean$", expected_results[['Brief description']]), ]

cat("1. CALCULATED MEANS FROM RAW DATA:\n")
# Calculate actual means for Plant Height
plant_height_means <- repellency_data %>%
  filter(Endpoint == "Plant height") %>%
  group_by(Dose) %>%
  summarise(
    calculated_mean = mean(Response, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  arrange(as.numeric(Dose))

print(kable(plant_height_means, digits = 6, caption = "Calculated Plant Height Means"))

# Calculate actual means for Shoot Dry Weight  
shoot_weight_means <- repellency_data %>%
  filter(Endpoint == "Shoot dry weight") %>%
  group_by(Dose) %>%
  summarise(
    calculated_mean = mean(Response, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  arrange(as.numeric(Dose))

cat("\n")
print(kable(shoot_weight_means, digits = 6, caption = "Calculated Shoot Dry Weight Means"))

cat("\n2. EXPECTED MEANS FROM test_cases_res:\n")
# Show expected results organized by endpoint
plant_height_expected <- expected_means[expected_means$Endpoint == "Plant height", ]
shoot_weight_expected <- expected_means[expected_means$Endpoint == "Shoot dry weight", ]

cat("Plant Height Expected:\n")
plant_height_summary <- plant_height_expected[, c("Brief description", "Dose", "expected result value")]
print(kable(plant_height_summary, digits = 6, caption = "Expected Plant Height Means"))

cat("\nShoot Dry Weight Expected:\n")
shoot_weight_summary <- shoot_weight_expected[, c("Brief description", "Dose", "expected result value")]
print(kable(shoot_weight_summary, digits = 6, caption = "Expected Shoot Dry Weight Means"))

cat("\n3. ALIGNMENT COMPARISON:\n")
# Create comparison for Plant Height
cat("PLANT HEIGHT ALIGNMENT:\n")
plant_comparison <- data.frame(
  Row = 1:nrow(plant_height_expected),
  Expected_Dose = plant_height_expected$Dose,
  Expected_Value = as.numeric(plant_height_expected[['expected result value']]),
  Description = plant_height_expected[['Brief description']],
  stringsAsFactors = FALSE
)

# Match with calculated means
plant_comparison$Calculated_Match_Dose <- NA
plant_comparison$Calculated_Value <- NA
plant_comparison$Difference <- NA

for(i in 1:nrow(plant_comparison)) {
  expected_val <- plant_comparison$Expected_Value[i]
  # Find the closest calculated mean
  best_match_idx <- which.min(abs(plant_height_means$calculated_mean - expected_val))
  if(length(best_match_idx) > 0) {
    plant_comparison$Calculated_Match_Dose[i] <- plant_height_means$Dose[best_match_idx]
    plant_comparison$Calculated_Value[i] <- plant_height_means$calculated_mean[best_match_idx]
    plant_comparison$Difference[i] <- abs(expected_val - plant_height_means$calculated_mean[best_match_idx])
  }
}

print(kable(plant_comparison, digits = 6, caption = "Plant Height Alignment Analysis"))

cat("\n4. ROW SHIFT TEST FOR PLANT HEIGHT:\n")
# Test if shifting expected results fixes alignment
doses_calculated <- sort(as.numeric(plant_height_means$Dose))
cat("Calculated doses (sorted):", paste(doses_calculated, collapse = ", "), "\n")

# Extract the dose pattern from expected descriptions
expected_descriptions <- plant_height_expected[['Brief description']]
cat("Expected descriptions:\n")
for(i in 1:length(expected_descriptions)) {
  cat(i, ":", expected_descriptions[i], "\n")
}

cat("\nExpected doses in order:", paste(plant_height_expected$Dose, collapse = ", "), "\n")

# Test alignment by matching with calculated dose order
cat("\nTesting if expected values match calculated means when properly ordered:\n")
alignment_test <- data.frame(
  Calculated_Dose = plant_height_means$Dose,
  Calculated_Mean = plant_height_means$calculated_mean,
  Expected_Value = plant_height_expected[['expected result value']][1:nrow(plant_height_means)],
  Difference = NA,
  stringsAsFactors = FALSE
)

alignment_test$Expected_Value <- as.numeric(alignment_test$Expected_Value)
alignment_test$Difference <- abs(alignment_test$Calculated_Mean - alignment_test$Expected_Value)

print(kable(alignment_test, digits = 6, caption = "Direct Order Alignment Test"))

# Check if the issue is in dose ordering
cat("\n5. DOSE ORDERING ISSUE ANALYSIS:\n")
cat("The issue appears to be that doses in expected results are not properly sorted.\n")
cat("Expected dose order in test_cases_res:", paste(plant_height_expected$Dose, collapse = ", "), "\n")
cat("Actual dose order from data (sorted):", paste(plant_height_means$Dose, collapse = ", "), "\n")

# Test different sorting approaches
expected_numeric_doses <- as.numeric(plant_height_expected$Dose)
expected_sorted_indices <- order(expected_numeric_doses)
cat("If we sort expected by numeric dose, the order should be:", paste(expected_sorted_indices, collapse = ", "), "\n")

corrected_alignment <- data.frame(
  Calculated_Dose = plant_height_means$Dose,
  Calculated_Mean = plant_height_means$calculated_mean,
  Expected_Value_Corrected = as.numeric(plant_height_expected[['expected result value']])[expected_sorted_indices],
  Difference_Corrected = NA,
  stringsAsFactors = FALSE
)
corrected_alignment$Difference_Corrected <- abs(corrected_alignment$Calculated_Mean - corrected_alignment$Expected_Value_Corrected)

print(kable(corrected_alignment, digits = 6, caption = "Corrected Alignment (Expected Sorted by Dose)"))

# Create plots showing the alignment issue
cat("\n6. CREATING ALIGNMENT PLOTS:\n")

# Plot showing the alignment issue
plot_data <- data.frame(
  Dose = as.numeric(plant_height_means$Dose),
  Calculated = plant_height_means$calculated_mean,
  Expected_Original = as.numeric(plant_height_expected[['expected result value']]),
  Expected_Corrected = as.numeric(plant_height_expected[['expected result value']])[expected_sorted_indices]
)

p1 <- ggplot(plot_data, aes(x = Dose)) +
  geom_point(aes(y = Calculated, color = "Calculated"), size = 3) +
  geom_point(aes(y = Expected_Original, color = "Expected (Original)"), size = 3) +
  geom_point(aes(y = Expected_Corrected, color = "Expected (Corrected)"), size = 3) +
  geom_line(aes(y = Calculated, color = "Calculated")) +
  geom_line(aes(y = Expected_Original, color = "Expected (Original)")) +
  geom_line(aes(y = Expected_Corrected, color = "Expected (Corrected)")) +
  scale_color_manual(values = c("Calculated" = "blue", "Expected (Original)" = "red", "Expected (Corrected)" = "green")) +
  labs(title = "Plant Height: Calculated vs Expected Alignment",
       x = "Dose", y = "Mean Plant Height",
       color = "Data Source") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

cat("\n7. SUMMARY:\n")
cat("The alignment issue is caused by incorrect dose ordering in the expected results.\n")
cat("When expected values are sorted by dose to match the calculated data order,\n")
cat("the alignment improves significantly.\n")
cat("Maximum difference with original ordering:", max(alignment_test$Difference, na.rm = TRUE), "\n")
cat("Maximum difference with corrected ordering:", max(corrected_alignment$Difference_Corrected, na.rm = TRUE), "\n")