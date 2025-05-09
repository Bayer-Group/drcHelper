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

# Define parameter grid for full simulation
param_grid <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4, 5),
  k_individuals = 10,
  var_tank = c(2, 4),
  var_individual = 2,
  max_effect = c(5, 20),
  response_type = c("decreasing", "none","threshold","oscillating"),
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)
nrow(param_grid)

full_results <- run_multiple_simulations_with_logging(
  param_grid = param_grid,
  test_methods = test_methods,
  n_sim = 1000
)
saveRDS(full_results,file="sim1_full_results.rds")
## Save the output into an R data object.

# Plot results for different scenarios
ggplot2::ggplot(full_results %>% dplyr::filter(max_effect==5,m_tanks==4), ggplot2::aes(x = Dose, y = Power, color = Method, group = Method)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_grid(response_type ~ var_tank,
                     labeller = ggplot2::labeller(max_effect = function(x) paste("Effect Size:", x))) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = paste("Power Analysis Across Scenarios:", "max_effect =", 5,", m_tanks =",4))

ggplot2::ggplot(full_results %>% dplyr::filter(max_effect==20,m_tanks==4), ggplot2::aes(x = Dose, y = Power, color = Method, group = Method)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_grid(response_type ~ var_tank,
                      labeller = ggplot2::labeller(effect_size = function(x) paste("Effect Size:", x))) +
  ggplot2::theme_bw() +
  ggplot2::labs(title = paste("Power Analysis Across Scenarios:", "max_effect =", 20,", m_tanks =",4))


ggplot2::ggplot(full_results %>% dplyr::filter(max_effect==20,m_tanks==4) %>% dplyr::filter(response_type =="oscillating"),
                ggplot2::aes(x = Expected_Response, y = Power, color = Method, group = Method)
                )+ggplot2::geom_point() +
  ggplot2::facet_grid(Dose_Level ~ var_tank,
                      labeller = ggplot2::labeller(max_effect = function(x) paste("Effect Size:", x))) +
  ggplot2::theme_bw()
