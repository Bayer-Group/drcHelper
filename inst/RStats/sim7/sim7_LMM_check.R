setwd("~/Projects/drcHelper/inst/RStats/sim7/")
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
  "LMM_Homo" = lmm_dunnett_homo
)
# oscillating_sim <- run_power_simulation(
#   n_sim = 1000,
#   n_doses = 5,
#   dose_range = c(0, 20),
#   m_tanks = 4,
#   k_individuals = 6,
#   var_tank = 4,
#   var_individual = 2,
#   max_effect = 0,
#   response_type = "none",  # New response type
#   test_methods = test_methods_o,
#   alternative = "less",  # For oscillating patterns, two-sided tests are appropriate
#   n_cores = 6
# )
# plotit <- FALSE
# if(plotit){
#   oscillating_sim <- get("oscillating_sim",env=RStats_check_osci_results)
#   plot_power_results(oscillating_sim)
# }




setwd("~/Projects/drcHelper/inst/RStats/sim7")
library(parallel)
library(doParallel)
library(drcHelper)
source("~/Projects/drcHelper/inst/RStats/RStats_wrapper_tests.R")
source("~/Projects/drcHelper/inst/RStats/RStats_Sim_Engine.R")
# Clean up any existing connections before starting
closeAllConnections()
gc()  # Force garbage collection
# Define test methods to compare
test_methods <- list(
  "LMM_Homo" = lmm_dunnett_homo
)


# ICC values to explore
icc_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)  # Low to high ICC

# Total variance (keeping this constant helps with interpretation)
total_variance <- 10

# Function to calculate variance components from ICC
calculate_variance_components <- function(icc, total_variance) {
  var_tank <- icc * total_variance
  var_individual <- (1 - icc) * total_variance
  return(list(var_tank = var_tank, var_individual = var_individual))
}

# Create variance component values for each ICC
variance_components <- lapply(icc_values, calculate_variance_components,
                              total_variance = total_variance)

# Extract as vectors for parameter grid
var_tank_values <- sapply(variance_components, function(x) x$var_tank)
var_individual_values <- sapply(variance_components, function(x) x$var_individual)

# Create named vectors for better readability in results
names(var_tank_values) <- paste0("ICC_", icc_values)
names(var_individual_values) <- paste0("ICC_", icc_values)

# Display the variance components
variance_table <- data.frame(
  ICC = icc_values,
  var_tank = var_tank_values,
  var_individual = var_individual_values,
  total_variance = total_variance
)
print(variance_table)



param_grid_2 <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4, 6),
  k_individuals = c(3,6,10),
  # var_tank = c(2, 4),
  # var_individual = c(4,6),
  max_effect = 0,
  response_type = c("none"),
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)

param_grid <- dplyr::cross_join(param_grid_2,variance_table)
dim(param_grid)
full_results <- run_multiple_simulations_with_logging(
  param_grid = param_grid,
  test_methods = test_methods,
  n_sim = 1000
)
saveRDS(full_results,file="sim7_full_results.rds")
## Save the output into an R data object.

########################################################################
plotit <- FALSE
if(plotit){
  # Plot results for different scenarios

  library(ggplot2)
  library(tidyverse)
  theme_set(theme_bw())
  full_results <- dplyr::left_join(param_grid,sim7_full_results) %>% mutate(k_individuals = factor(k_individuals))

        ggplot(full_results,aes(x=ICC,y=Power,color=k_individuals))+ geom_point()+
          facet_grid(m_tanks ~ Dose_Level,scales = "free")  +
          geom_line()+
          geom_hline(yintercept = c(0.05),lty=2,alpha=0.3)+
          theme(legend.position = "bottom")+ ## ggplot2::scale_color_viridis_d(direction = 1)
          ggthemes::scale_color_solarized()+ggtitle("LMM does not preserve alpha")
        ggsave("SimPower_LMM_alpha.png",dpi=300, width = 6,height =5)






}
