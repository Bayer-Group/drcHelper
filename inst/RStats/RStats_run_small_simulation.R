library(parallel)
library(doParallel)
library(drcHelper)
source("~/Projects/drcHelper/inst/RStats/RStats_wrapper_tests.R")
source("~/Projects/drcHelper/inst/RStats/RStats_Sim_Engine.R")
# Define test methods to compare
test_methods <- list(
  "LMM_Homo" = lmm_dunnett_homo,
  "LMM_Hetero" = lmm_dunnett_hetero,
  "LM_Homo" = lm_dunnett_agg,
  "LM_Hetero" = gls_dunnett_agg,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test,
  "Dunns" = manyone_dunns_test,
  "Wilcoxon" = manyone_wilcox_test
)

ncores <- detectCores()
# Run a small-scale simulation first
small_sim <- run_power_simulation(
  n_sim = 10,  # Small number for testing
  n_doses = 4,
  dose_range = c(0, 20),
  m_tanks = 3,
  k_individuals = 6,
  var_tank = 4,
  var_individual = 2,
  max_effect = 20,  # Moderate effect size
  response_type = "decreasing",
  test_methods = test_methods,
  alternative = "less",
  n_cores = 3  # Use fewer cores for testing
)

# Plot results
plot_power_results(small_sim)

small_sim <- run_power_simulation(
  n_sim = 10,  # Small number for testing
  n_doses = 4,
  dose_range = c(0, 20),
  m_tanks = 3,
  k_individuals = 6,
  var_tank = 4,
  var_individual = 2,
  max_effect = 20,  # Moderate effect size
  response_type = "threshold",
  test_methods = test_methods,
  alternative = "less",
  n_cores = 3  # Use fewer cores for testing
)

# Plot results
plot_power_results(small_sim)

# Run a simulation with oscillating response

test_methods_o <- list(
  "LMM_Homo" = lmm_dunnett_homo,
  "LM_Homo" = lm_dunnett_agg,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test,
)
oscillating_sim <- run_power_simulation(
  n_sim = 10,
  n_doses = 5,
  dose_range = c(0, 20),
  m_tanks = 4,
  k_individuals = 6,
  var_tank = 4,
  var_individual = 2,
  max_effect = 20,
  response_type = "oscillating",  # New response type
  test_methods = test_methods_o,
  alternative = "less",  # For oscillating patterns, two-sided tests are appropriate
  n_cores = 3
)
plot_power_results(oscillating_sim)


# Define parameter grid for full simulation
param_grid <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(3, 5),
  k_individuals = 10,
  var_tank = c(2, 4),
  var_individual = 2,
  max_effect = c(5, 10, 20),
  response_type = c("decreasing", "non_monotonic", "none","threshold","oscillating"),
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)


# full_results <- run_multiple_simulations(
#   param_grid = param_grid,
#   test_methods = test_methods,
#   n_sim = 1000
# )

# Plot results for different scenarios
# ggplot2::ggplot(full_results, ggplot2::aes(x = Dose_Num, y = Power, color = Method, group = Method)) +
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::facet_grid(response_type ~ effect_size,
#                      labeller = ggplot2::labeller(effect_size = function(x) paste("Effect Size:", x))) +
#   ggplot2::theme_minimal() +
#   ggplot2::labs(title = "Power Analysis Across Scenarios")
