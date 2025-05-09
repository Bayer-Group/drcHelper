setwd("~/Projects/drcHelper/inst/RStats/sim6/")
# Define test methods focusing on tank-level data methods
library(parallel)
library(doParallel)
library(drcHelper)
source("~/Projects/drcHelper/inst/RStats/RStats_wrapper_tests.R")
source("~/Projects/drcHelper/inst/RStats/RStats_Sim_Engine.R")
source("~/Projects/drcHelper/inst/RStats/sim5/hetero_simfun.R")

# Clean up any existing connections before starting
closeAllConnections()
gc()  # Force garbage

tank_level_methods <- list(
  "Jonckheere" = jonckheere_test
)


test_JT_sim_4 <- run_hetero_variance_simulation(
  n_sim = 1000,  # Small number for testing
  n_doses = 7,
  m_tanks = 4,
  variance_pattern = c(2,2,2,2,10,2,4),
  max_effect = 10,
  response_type = "threshold",
  threshold_idx = 5,
  test_methods = tank_level_methods,
  n_cores = parallel::detectCores() - 1
)

saveRDS(test_JT_sim_4 ,file="test_JT_sim_4.rds")
plotit <- FALSE
if(plotit){
  plot_hetero_variance_results <- function(results) {
    # Create a plot showing power by dose level for each method and variance pattern
    ggplot2::ggplot(results,
                    ggplot2::aes(x = Dose, y = Power, color = Method, group = Method)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_grid(response_type ~ variance_pattern,
                          labeller = ggplot2::labeller(
                            response_type = function(x) paste0("Response: ", x),
                            variance_pattern = function(x) paste0("Variance: ", x)
                          )) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Power Analysis with Heterogeneous Variances",
        subtitle = paste("Tanks per dose:", results$m_tanks[1],
                         "| Max effect:", results$max_effect[1]),
        x = "Dose",
        y = "Power (Proportion of Significant Results)"
      )
  }
  # Plot test results
  test_plot <- plot_hetero_variance_results(test_JT_sim_4)
  print(test_plot)

}



