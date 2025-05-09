# 1. Variance Components and Hierarchical Structure

# Variance component relationships
## Using ICC instead



# Number of individuals per tank
k_individuals_values <- c(3, 6, 10)  # Few, moderate, many individuals per tank

# Number of tanks per dose
m_tanks_values <- c(4, 6)  # Typical values in ecotoxicology studies


# 2. Response patterns
response_types <- c("none", "decreasing", "threshold", "oscillating") ## do we want non_monotonic too?

# Maximum effect sizes (% change from control)
max_effect_values <- c(5, 10, 20)  # small, moderate, large

# For threshold response
threshold_idx_values <- 3  # Effect starts at 2nd or 3rd dose level

# Dose structure
n_doses <- 5  # Standard number of dose levels including control
dose_range <- c(0, 20)  # Typical range



# 3. Variance patterns (for tank-level data)
variance_patterns <- list(
  homogeneous = function(dose_idx, n_doses) rep(1, n_doses),
  increasing = function(dose_idx, n_doses) seq(1, 3, length.out = n_doses),
  decreasing = function(dose_idx, n_doses) seq(3, 1, length.out = n_doses),
  v_shaped = function(dose_idx, n_doses) {
    mid <- ceiling(n_doses/2)
    c(seq(3, 1, length.out = mid), seq(1, 3, length.out = n_doses - mid + 1)[-1])
  }
)

## For tank-level data

# 4. Test methods to compare
test_methods_tank <- list(
  "LM_Homo" = lm_dunnett_agg,
  "LM_Hetero" = gls_dunnett_agg,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test,
  "Dunns" = dunns_test
)

# 5. Simulation control parameters
n_sim_values <- c(100, 1000)  # For testing and final runs
alpha <- 0.05  # Standard significance level




# For Question 1: Variance components and hierarchical structure
param_grid_q1 <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = m_tanks_values,
  k_individuals = k_individuals_values,
  var_tank = var_tank_values,
  var_individual = var_individual_values,
  max_effect = 20,  # Fixed moderate effect
  response_type = "decreasing",  # Fixed decreasing response
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)

# For Question 2: Dose-response patterns and effect sizes

test_methods_tank <- list(
  "LM_Homo" = lm_dunnett_agg,
  "LM_Hetero" = gls_dunnett_agg,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test,
  "Dunns" = dunns_test
)
response_types <- c("none", "decreasing", "threshold", "oscillating") ## do we want non_monotonic too?

param_grid_q2 <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4,6,10),  # Fixed moderate number of tanks
  k_individuals = 1,  # Fixed moderate number of individuals
  var_tank = c(0.5,1,3,5,9,10),  # Fixed moderate tank variance
  var_individual = 0,  # Fixed no individual variance
  max_effect = c(5,10,20),
  response_type = response_types,
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)
nrow(param_grid_q2) ## 216

# For Question 3: Variance homogeneity/heterogeneity
# This requires a special approach to create dose-specific variances
# We'll implement this separately in the simulation function

# Example function to create dose-specific variance vectors
create_variance_pattern <- function(pattern_name, n_doses, base_variance = 4) {
  patterns <- list(
    homogeneous = rep(base_variance, n_doses),
    increasing = base_variance * seq(0.5, 2, length.out = n_doses),
    decreasing = base_variance * seq(2, 0.5, length.out = n_doses),
    v_shaped = base_variance * c(seq(2, 0.5, length.out = ceiling(n_doses/2)),
                                 seq(0.5, 2, length.out = floor(n_doses/2) + 1)[-1])
  )

  return(patterns[[pattern_name]])
}

# Parameter grid for heterogeneous variance question
variance_patterns <- c("homogeneous", "increasing", "decreasing", "v_shaped")

create_variance_pattern(variance_patterns[4],n_doses=5)

param_grid_q3 <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4,6),
  k_individuals = 1,
  variance_pattern = variance_patterns,
  max_effect = c(5, 20),  # Null and moderate effect
  response_type = c("none", "decreasing"),  # Focus on null and decreasing
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)




# Example visualization code for ICC impact
plot_icc_impact <- function(results) {
  # Filter to focus on specific response type and effect size
  filtered_results <- results[results$response_type == "decreasing" &
                                results$max_effect == 20, ]

  # Create plot
  ggplot2::ggplot(filtered_results,
                  ggplot2::aes(x = factor(ICC), y = Power, color = Method, group = Method)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_grid(k_individuals ~ m_tanks,
                        labeller = ggplot2::labeller(
                          k_individuals = function(x) paste0("Individuals: ", x),
                          m_tanks = function(x) paste0("Tanks: ", x)
                        )) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Impact of ICC on Power by Method",
      subtitle = "Decreasing Dose-Response with Moderate Effect",
      x = "Intraclass Correlation Coefficient (ICC)",
      y = "Power"
    )
}
