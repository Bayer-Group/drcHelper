# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # For combining plots



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # For combining plots

# Create the simulated fish data
simulated_fish_data <- tibble::tibble(
  treatment = c(rep("Control", 8), rep("Low", 4), rep("Medium", 4), rep("High", 4)),
  tank = c(paste0("C", 1:8), paste0("L", 1:4), paste0("M", 1:4), paste0("H", 1:4)),
  S0 = c(3, 2, 4, 3, 2, 1, 2, 3, 3, 3, 4, 2, 2, 3, 2, 2, 2, 3, 1, 2),
  S1 = c(1, 2, 0, 1, 2, 2, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 2, 0),
  S2 = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1),
  S3 = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1),
  total = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
)

# Ensure treatment is an ordered factor for proper plotting
simulated_fish_data$treatment <- factor(simulated_fish_data$treatment,
                                        levels = c("Control", "Low", "Medium", "High"))

# Convert to long format for easier plotting
fish_data_long <- simulated_fish_data %>%
  pivot_longer(
    cols = starts_with("S"),
    names_to = "score",
    values_to = "count"
  ) %>%
  # Ensure score is an ordered factor
  mutate(score = factor(score, levels = c("S0", "S1", "S2", "S3")))

# 1. Stacked Bar Plot by Treatment Group
p1 <- fish_data_long %>%
  group_by(treatment, score) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = treatment, y = total_count, fill = score)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribution of Injury Scores by Treatment Group",
    x = "Treatment Group",
    y = "Number of Fish",
    fill = "Injury Score"
  ) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "right"
  )

# 2. Proportion Plot by Treatment Group
p2 <- fish_data_long %>%
  group_by(treatment, score) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ggplot(aes(x = treatment, y = proportion, fill = score)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Proportion of Injury Scores by Treatment Group",
    x = "Treatment Group",
    y = "Proportion of Fish",
    fill = "Injury Score"
  ) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "right"
  )

# 3. Boxplot of Injury Proportions by Treatment
p3 <- simulated_fish_data %>%
  mutate(
    injury_prop = (S1 + S2 + S3) / total,
    severe_injury_prop = (S2 + S3) / total
  ) %>%
  pivot_longer(
    cols = c(injury_prop, severe_injury_prop),
    names_to = "injury_type",
    values_to = "proportion"
  ) %>%
  mutate(injury_type = factor(injury_type,
                              levels = c("injury_prop", "severe_injury_prop"),
                              labels = c("Any Injury (S1-S3)", "Severe Injury (S2-S3)"))) %>%
  ggplot(aes(x = treatment, y = proportion, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  facet_wrap(~ injury_type) +
  labs(
    title = "Distribution of Injury Proportions by Treatment Group",
    x = "Treatment Group",
    y = "Proportion of Fish",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "none"
  )

# 4. Heat Map of Injury Counts by Tank
p4 <- fish_data_long %>%
  ggplot(aes(x = tank, y = score, fill = count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = count), color = "black") +
  facet_grid(. ~ treatment, scales = "free_x", space = "free_x") +
  labs(
    title = "Heat Map of Injury Counts by Tank",
    x = "Tank",
    y = "Injury Score",
    fill = "Count"
  ) +
  scale_fill_gradient(low = "white", high = "#fc8d62") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# 5. Dot plot showing proportion of each injury type by treatment
p5 <- fish_data_long %>%
  group_by(treatment, score) %>%
  summarize(
    total_count = sum(count),
    .groups = "drop"
  ) %>%
  group_by(treatment) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ggplot(aes(x = treatment, y = proportion, color = score)) +
  geom_point(size = 4) +
  geom_line(aes(group = score), linewidth = 1) +
  facet_wrap(~ score, nrow = 1) +
  labs(
    title = "Trend in Proportion of Each Injury Score Across Treatments",
    x = "Treatment Group",
    y = "Proportion of Fish",
    color = "Injury Score"
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_color_brewer(palette = "YlOrRd") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )

# 6. Stacked bar plot for each tank
p6 <- fish_data_long %>%
  ggplot(aes(x = tank, y = count, fill = score)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(. ~ treatment, scales = "free_x", space = "free_x") +
  labs(
    title = "Distribution of Injury Scores by Individual Tank",
    x = "Tank",
    y = "Number of Fish",
    fill = "Injury Score"
  ) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Combine plots using patchwork
top_row <- p1 + p2
middle_row <- p3
bottom_row <- p4 / p5
last_row <- p6

combined_plot <- (top_row) / (middle_row) / (bottom_row) / (last_row) +
  plot_layout(heights = c(1, 1, 2, 1)) +
  plot_annotation(
    title = "Visualization of Simulated Fish Injury Data",
    subtitle = "Multiple visualizations showing the distribution of injury scores across treatment groups and tanks",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  )

# Print the combined plot
print(combined_plot)

# Statistical summary table
summary_table <- simulated_fish_data %>%
  group_by(treatment) %>%
  summarize(
    n_tanks = n(),
    total_fish = sum(total),
    s0_count = sum(S0),
    s0_percent = 100 * s0_count / total_fish,
    s1_count = sum(S1),
    s1_percent = 100 * s1_count / total_fish,
    s2_count = sum(S2),
    s2_percent = 100 * s2_count / total_fish,
    s3_count = sum(S3),
    s3_percent = 100 * s3_count / total_fish,
    any_injury = sum(S1 + S2 + S3),
    any_injury_percent = 100 * any_injury / total_fish,
    .groups = "drop"
  ) %>%
  mutate(across(ends_with("percent"), ~ round(., 1)))

# Print the summary table
print(summary_table)

# Calculate injury severity index (weighted average of injury scores)
severity_by_treatment <- simulated_fish_data %>%
  mutate(
    severity_index = (0*S0 + 1*S1 + 2*S2 + 3*S3) / total
  ) %>%
  group_by(treatment) %>%
  summarize(
    mean_severity = mean(severity_index),
    sd_severity = sd(severity_index),
    .groups = "drop"
  )

# Print severity index
print(severity_by_treatment)



# Example data with increasing trend in injury rates
fish_data <- data.frame(
  tmt = rep(c("Control", "Low", "Medium", "High"), each = 3),
  tank = paste0(rep(c("Control", "Low", "Medium", "High"), each = 3),
                rep(1:3, times = 4)),
  S0 = c(9,8,9, 7,6,7, 5,4,5, 3,2,3),
  S1 = c(1,2,1, 3,4,3, 5,6,5, 7,8,7),
  total = rep(10, 12)
)

# Ensure treatment is an ordered factor for proper plotting
fish_data$tmt <- factor(fish_data$tmt, levels = c("Control", "Low", "Medium", "High"))

# Convert to long format for easier plotting
fish_data_long <- fish_data %>%
  pivot_longer(
    cols = starts_with("S"),
    names_to = "score",
    values_to = "count"
  ) %>%
  # Ensure score is an ordered factor
  mutate(score = factor(score, levels = paste0("S", 0:1)))

# 1. Stacked Bar Plot by Treatment Group
p1 <- ggplot(fish_data_long, aes(x = tmt, y = count, fill = score)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribution of Injury Scores by Treatment Group",
    x = "Treatment Group",
    y = "Number of Fish",
    fill = "Injury Score"
  ) +
  scale_fill_manual(values = c("S0" = "#66c2a5", "S1" = "#fc8d62")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "bottom"
  )

# 2. Proportion Plot by Treatment Group
p2 <- fish_data_long %>%
  group_by(tmt, score) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  group_by(tmt) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ggplot(aes(x = tmt, y = proportion, fill = score)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Proportion of Injury Scores by Treatment Group",
    x = "Treatment Group",
    y = "Proportion of Fish",
    fill = "Injury Score"
  ) +
  scale_fill_manual(values = c("S0" = "#66c2a5", "S1" = "#fc8d62")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "bottom"
  )

# 3. Individual Tank Data with Treatment Averages
p3 <- fish_data %>%
  mutate(S1_prop = S1 / total) %>%
  ggplot(aes(x = tmt, y = S1_prop)) +
  # Add individual tank points
  geom_point(aes(color = tank), position = position_jitter(width = 0.2), size = 3, alpha = 0.7) +
  # Add treatment group means
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") +
  stat_summary(fun = mean, geom = "line", group = 1, linewidth = 1, color = "black") +
  labs(
    title = "Proportion of S1 Injuries by Treatment Group",
    subtitle = "Individual tanks shown as points, group means as diamonds",
    x = "Treatment Group",
    y = "Proportion of S1 Injuries",
    color = "Tank"
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "bottom"
  )

# 4. Boxplot of S1 Proportions
p4 <- fish_data %>%
  mutate(S1_prop = S1 / total) %>%
  ggplot(aes(x = tmt, y = S1_prop)) +
  geom_boxplot(aes(fill = tmt), alpha = 0.7) +
  geom_point(position = position_jitter(width = 0.2), size = 2) +
  labs(
    title = "Boxplot of S1 Injury Proportions by Treatment",
    x = "Treatment Group",
    y = "Proportion of S1 Injuries"
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = "none"
  )

# 5. Heat Map of Injury Counts by Tank
p5 <- fish_data_long %>%
  ggplot(aes(x = tank, y = score, fill = count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = count), color = "black") +
  facet_grid(. ~ tmt, scales = "free_x", space = "free_x") +
  labs(
    title = "Heat Map of Injury Counts by Tank",
    x = "Tank",
    y = "Injury Score",
    fill = "Count"
  ) +
  scale_fill_gradient(low = "white", high = "#fc8d62") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# 6. Line Plot of S1+ Proportion Trend
p6 <- fish_data %>%
  group_by(tmt) %>%
  summarize(
    total_fish = sum(total),
    total_s1 = sum(S1),
    prop_s1 = total_s1 / total_fish,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = tmt, y = prop_s1, group = 1)) +
  geom_line(linewidth = 1.5, color = "#fc8d62") +
  geom_point(size = 4, color = "#fc8d62") +
  labs(
    title = "Trend in S1+ Injury Proportion",
    x = "Treatment Group",
    y = "Proportion of Fish with S1+ Injuries"
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12)
  )

# Combine plots using patchwork
top_row <- p1 + p2
middle_row <- p3 + p4
bottom_row <- p5 + p6

combined_plot <- top_row / middle_row / bottom_row +
  plot_layout(heights = c(1, 1, 1.2)) +
  plot_annotation(
    title = "Visualization of Fish Injury Data Across Treatment Groups",
    subtitle = "Multiple visualization approaches showing the increasing trend in injury rates",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  )

# Print the combined plot
combined_plot

# Save the plot if needed
# ggsave("fish_injury_visualizations.png", combined_plot, width = 12, height = 14, dpi = 300)

# Statistical summary table
summary_table <- fish_data %>%
  group_by(tmt) %>%
  summarize(
    n_tanks = n(),
    total_fish = sum(total),
    s0_count = sum(S0),
    s0_percent = 100 * s0_count / total_fish,
    s1_count = sum(S1),
    s1_percent = 100 * s1_count / total_fish,
    .groups = "drop"
  ) %>%
  mutate(across(ends_with("percent"), ~ round(., 1)))

# Print the summary table
print(summary_table)

# Run RSCA tests with different alternative hypotheses
run_threshold_RSCA <- function(data, threshold = 1, alternative = "two.sided") {
  # Simple implementation for demonstration
  affected <- data$S1  # For S1+ threshold

  # Calculate Z-statistic (simplified for demonstration)
  # This is a simplified calculation and not the full RSCA implementation
  tmt_numeric <- as.numeric(factor(data$tmt))
  prop_affected <- affected / data$total

  # Fit linear model for demonstration
  model <- lm(prop_affected ~ tmt_numeric)
  z_stat <- summary(model)$coefficients[2, 3]

  # Calculate p-value based on alternative
  p_value <- switch(alternative,
                    "two.sided" = 2 * pnorm(-abs(z_stat)),
                    "greater" = pnorm(-z_stat),
                    "less" = pnorm(z_stat))

  return(list(Z = z_stat, p_value = p_value, alternative = alternative))
}

# Run tests with different alternatives
two_sided_result <- run_threshold_RSCA(fish_data, alternative = "two.sided")
greater_result <- run_threshold_RSCA(fish_data, alternative = "greater")
less_result <- run_threshold_RSCA(fish_data, alternative = "less")

# Print results
cat("\nRSCA Test Results for S1+ Threshold:\n")
cat("Two-sided test: Z =", round(two_sided_result$Z, 3),
    ", p-value =", round(two_sided_result$p_value, 4), "\n")
cat("One-sided test (greater): Z =", round(greater_result$Z, 3),
    ", p-value =", round(greater_result$p_value, 4), "\n")
cat("One-sided test (less): Z =", round(less_result$Z, 3),
    ", p-value =", round(less_result$p_value, 4), "\n")


# Load necessary libraries
library(dplyr)
library(knitr)

# Create the simulated fish data
simulated_fish_data <- tibble::tibble(
  treatment = c(rep("Control", 8), rep("Low", 4), rep("Medium", 4), rep("High", 4)),
  tank = c(paste0("C", 1:8), paste0("L", 1:4), paste0("M", 1:4), paste0("H", 1:4)),
  S0 = c(3, 2, 3, 2, 2, 1, 2, 3, 3, 3, 4, 2, 2, 3, 2, 2, 2, 3, 1, 2),
  S1 = c(1, 2, 0, 1, 2, 2, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 2, 0),
  S2 = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1),
  S3 = c(0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1),
  total = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
)

# Ensure treatment is an ordered factor for proper filtering
simulated_fish_data$treatment <- factor(simulated_fish_data$treatment,
                                        levels = c("Control", "Low", "Medium", "High"))

# Define a function to implement the run_all_threshold_tests function
# This is a simplified implementation for demonstration
run_all_threshold_tests <- function(data, min_score = 1, max_score = 3,
                                    alternative = "greater", direction = "greater") {
  # Find score columns
  score_cols <- grep("^S[0-9]$", names(data), value = TRUE)

  # Initialize results dataframe
  results <- data.frame(
    Threshold = character(),
    Z_statistic = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )

  # For each threshold
  for (threshold in min_score:max_score) {
    # Determine which scores to include based on direction
    if (direction == "greater") {
      selected_cols <- score_cols[as.numeric(gsub("S", "", score_cols)) >= threshold]
      threshold_label <- paste0("S", threshold, "+")
    } else {
      selected_cols <- score_cols[as.numeric(gsub("S", "", score_cols)) <= threshold]
      threshold_label <- paste0("S≤", threshold)
    }

    # Calculate affected counts for each tank
    affected <- rowSums(data[, selected_cols, drop = FALSE])
    total <- data$total

    # Calculate proportions by treatment
    props <- aggregate(
      cbind(affected = affected, total = total) ~ treatment,
      data = data,
      FUN = sum
    )
    props$proportion <- props$affected / props$total

    # Simple trend test (linear model on proportions)
    # This is a simplified calculation, not the full RSCA implementation
    trt_numeric <- as.numeric(props$treatment)
    model <- lm(proportion ~ trt_numeric, data = props)
    z_stat <- summary(model)$coefficients[2, 3]  # t-statistic as proxy for Z

    # Calculate p-value based on alternative
    p_value <- switch(alternative,
                      "two.sided" = 2 * pt(-abs(z_stat), df = nrow(props) - 2),
                      "greater" = pt(-z_stat, df = nrow(props) - 2),
                      "less" = pt(z_stat, df = nrow(props) - 2))

    # Add to results
    results <- rbind(results, data.frame(
      Threshold = threshold_label,
      Z_statistic = z_stat,
      P_value = p_value,
      stringsAsFactors = FALSE
    ))
  }

  return(results)
}

# Perform step-down procedure
perform_step_down <- function(data, min_score = 1, max_score = 3,
                              alternative = "greater", direction = "greater") {
  # Get ordered treatment levels
  treatments <- levels(data$treatment)
  n_treatments <- length(treatments)

  # Initialize list to store results
  step_results <- list()

  # Perform tests for each step
  for (i in 1:(n_treatments-1)) {
    # Select treatments for this step
    included_treatments <- treatments[1:(n_treatments-i+1)]
    step_data <- data %>% filter(treatment %in% included_treatments)

    # Run tests
    results <- run_all_threshold_tests(
      step_data,
      min_score = min_score,
      max_score = max_score,
      alternative = alternative,
      direction = direction
    )

    # Add step information
    results$Step <- i
    results$Included_Treatments <- paste(included_treatments, collapse = ", ")
    results$Highest_Treatment <- included_treatments[length(included_treatments)]

    # Store results
    step_results[[i]] <- results
  }

  # Combine all results
  combined_results <- do.call(rbind, step_results)

  return(combined_results)
}

# Run the step-down procedure
step_down_results <- perform_step_down(
  simulated_fish_data,
  min_score = 1,
  max_score = 3,
  alternative = "greater",
  direction = "greater"
)

# Format the results for better display
formatted_results <- step_down_results %>%
  mutate(
    Z_statistic = round(Z_statistic, 3),
    P_value = round(P_value, 4),
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01 ~ "**",
      P_value < 0.05 ~ "*",
      P_value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  select(Step, Highest_Treatment, Included_Treatments, Threshold, Z_statistic, P_value, Significance)

# Print formatted results
cat("## Step-Down Procedure Results\n\n")
print(kable(formatted_results, caption = "RSCA Test Results by Step and Threshold"))

# Create a visualization of the step-down results
library(ggplot2)

# Plot p-values by step and threshold
ggplot(step_down_results, aes(x = Threshold, y = Step, fill = -log10(P_value))) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.4f", round(P_value, 4))), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue",
                      name = "-log10(p-value)") +
  scale_y_reverse(breaks = 1:3,
                  labels = c("All treatments",
                             "Excluding High",
                             "Control vs Low only")) +
  labs(
    title = "Step-Down Procedure Results",
    subtitle = "P-values for each threshold at each step",
    x = "Injury Threshold",
    y = "Step"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# Create a summary of the step-down procedure
cat("\n\n## Step-Down Procedure Summary\n\n")
cat("The step-down procedure systematically excludes the highest treatment groups to identify at which level the dose-response relationship becomes significant.\n\n")

cat("### Step 1: All Treatment Groups\n")
step1 <- step_down_results %>% filter(Step == 1)
cat("Included treatments: ", unique(step1$Included_Treatments), "\n")
sig_thresholds1 <- step1 %>% filter(P_value < 0.05) %>% pull(Threshold)
if(length(sig_thresholds1) > 0) {
  cat("Significant thresholds: ", paste(sig_thresholds1, collapse = ", "), "\n\n")
} else {
  cat("No significant thresholds found.\n\n")
}

cat("### Step 2: Excluding Highest Treatment Group\n")
step2 <- step_down_results %>% filter(Step == 2)
cat("Included treatments: ", unique(step2$Included_Treatments), "\n")
sig_thresholds2 <- step2 %>% filter(P_value < 0.05) %>% pull(Threshold)
if(length(sig_thresholds2) > 0) {
  cat("Significant thresholds: ", paste(sig_thresholds2, collapse = ", "), "\n\n")
} else {
  cat("No significant thresholds found.\n\n")
}

cat("### Step 3: Control vs. Lowest Treatment Group Only\n")
step3 <- step_down_results %>% filter(Step == 3)
cat("Included treatments: ", unique(step3$Included_Treatments), "\n")
sig_thresholds3 <- step3 %>% filter(P_value < 0.05) %>% pull(Threshold)
if(length(sig_thresholds3) > 0) {
  cat("Significant thresholds: ", paste(sig_thresholds3, collapse = ", "), "\n\n")
} else {
  cat("No significant thresholds found.\n\n")
}

cat("### Overall Conclusion\n")
lowest_sig_step <- min(step_down_results %>% filter(P_value < 0.05) %>% pull(Step), Inf)
if(is.infinite(lowest_sig_step)) {
  cat("No significant dose-response relationship was detected at any step.\n")
} else {
  lowest_sig_treatment <- step_down_results %>%
    filter(Step == lowest_sig_step) %>%
    pull(Highest_Treatment) %>%
    unique()
  lowest_sig_threshold <- step_down_results %>%
    filter(Step == lowest_sig_step, P_value < 0.05) %>%
    arrange(P_value) %>%
    pull(Threshold) %>%
    first()

  cat("The lowest treatment level at which a significant dose-response relationship was detected is: ",
      lowest_sig_treatment, " (threshold: ", lowest_sig_threshold, ").\n", sep = "")
}


