# Install and load necessary packages if not already installed
if (!requireNamespace("multcomp", quietly = TRUE)) {
  install.packages("multcomp")
}
library(multcomp)

# Set seed for reproducibility
set.seed(123)

# Simulate a dose-response dataset
# Assume fluorescence measurements for a control and 4 dose levels
dose_levels <- factor(c(rep("Control", 6), rep("Dose1", 6), rep("Dose2", 6),
                        rep("Dose3", 6), rep("Dose4", 6)))
fluorescence <- c(rnorm(6, mean=100, sd=5),  # Control
                  rnorm(6, mean=98, sd=5),   # Dose1 (slight decrease)
                  rnorm(6, mean=95, sd=5),   # Dose2 (moderate decrease)
                  rnorm(6, mean=92, sd=5),   # Dose3 (larger decrease)
                  rnorm(6, mean=90, sd=5))   # Dose4 (largest decrease)

# Create a data frame
data <- data.frame(Dose = dose_levels, Fluorescence = fluorescence)

# Fit a linear model (ANOVA) for the dose effect
model <- lm(Fluorescence ~ Dose, data = data)

# Perform Dunnett's test (two-tailed by default)
dunnett_2tailed <- glht(model, linfct = mcp(Dose = "Dunnett"))
summary_dunnett_2tailed <- summary(dunnett_2tailed)

# Perform Dunnett's test (one-tailed, for decrease, i.e., treatment < control)
# Use alternative = "less" for one-tailed test expecting decrease
dunnett_1tailed <- glht(model, linfct = mcp(Dose = "Dunnett"), alternative = "less")
summary_dunnett_1tailed <- summary(dunnett_1tailed)

# Display the dataset
cat("Simulated Dose-Response Dataset:\n")
print(head(data, 10))
cat("\nSummary Statistics by Dose Level:\n")
print(aggregate(Fluorescence ~ Dose, data = data, mean))

# Display results for two-tailed Dunnett's test
cat("\nTwo-Tailed Dunnett's Test Results:\n")
print(summary_dunnett_2tailed)

# Display results for one-tailed Dunnett's test (expecting decrease)
cat("\nOne-Tailed Dunnett's Test Results (testing for decrease):\n")
print(summary_dunnett_1tailed)
