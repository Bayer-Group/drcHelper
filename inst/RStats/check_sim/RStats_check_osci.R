setwd("~/Projects/drcHelper/inst/RStats/check_osci/")
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

test_methods_o <- list(
  "LMM_Homo" = lmm_dunnett_homo,
  "LM_Homo" = lm_dunnett_agg,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test
)
oscillating_sim <- run_power_simulation(
  n_sim = 100,
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
  n_cores = 6
)
plotit <- FALSE
if(plotit){
  oscillating_sim <- get("oscillating_sim",env=RStats_check_osci_results)
  plot_power_results(oscillating_sim)
}




