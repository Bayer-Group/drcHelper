setwd("~/Projects/drcHelper/inst/RStats/check_sim/")
library(parallel)
library(doParallel)
library(drcHelper)
source("~/Projects/drcHelper/inst/RStats/RStats_wrapper_tests.R")
source("~/Projects/drcHelper/inst/RStats/RStats_Sim_Engine.R")
# Define test methods to compare
# Clean up any existing connections before starting
closeAllConnections()
gc()  # Force garbage collection
# Run a simulation with oscillating response

tank_level_methods <- list(
  "LM_Homo" = lm_dunnett_agg_simple,
  "LM_Hetero" = gls_dunnett_agg_simple,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test
)




# Parameter grid for heterogeneous variance question
param_grid_hetero <- expand.grid(
  n_doses = 7,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4,6),
  variance_pattern = c(2,2,2,2,10,4),
  max_effect = c(0),  # Null and moderate effect
  response_type = c("none"),  # Focus on null and decreasing
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)

nrow(param_grid_hetero)

test_hetero_sim <- run_hetero_variance_simulation(
  n_sim = 10,  # Small number for testing
  n_doses = 5,
  m_tanks = 3,
  variance_pattern = c(2,2,2,2,10,4),
  max_effect = 5,
  response_type = "decreasing",
  test_methods = tank_level_methods,
  n_cores = parallel::detectCores() - 1
)

sim5_results <- run_multiple_hetero_simulations(param_grid=param_grid_hetero,
                                                test_methods=tank_level_methods,
                                                n_sim = 1000,
                                                n_cores = parallel::detectCores() - 1,
                                                seed = 123)

plotit <- FALSE
if(plotit){
  none_sim <- get("none_sim",env=RStats_check_none_results)
  plot_power_results(none_sim)
}




