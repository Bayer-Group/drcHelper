setwd("~/Projects/drcHelper/inst/RStats/sim4")
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
  "LMM_Homo" = lmm_dunnett_homo,
  "LMM_Hetero" = lmm_dunnett_hetero,
  "LM_Homo" = lm_dunnett_agg,
  "LM_Hetero" = gls_dunnett_agg,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test,
  "Dunns" = manyone_dunns_test
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


# Define parameter grid for full simulation
param_grid_1 <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4, 6),
  k_individuals = c(3,6,10),
  # var_tank = c(2, 4),
  # var_individual = c(4,6),
  max_effect = c(5, 20),
  response_type = c("threshold","non_monotonic","oscillating"),
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)

param_grid_2 <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4, 6),
  k_individuals = c(3,6,10),
  # var_tank = c(2, 4),
  # var_individual = c(4,6),
  max_effect = c(5),
  response_type = c("none"),
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)

nrow(param_grid_1)
nrow(param_grid_2)
param_grid_1 <- rbind(param_grid_1,param_grid_2)
(36+6)*5 ## 180 scenarios, 4 non-control, ,
param_grid <- dplyr::cross_join(param_grid_1,variance_table)
dim(param_grid)
full_results <- run_multiple_simulations_with_logging(
  param_grid = param_grid,
  test_methods = test_methods,
  n_sim = 1000
)
saveRDS(full_results,file="sim4_full_results.rds")
## Save the output into an R data object.

########################################################################
plotit <- FALSE
if(plotit){
  # Plot results for different scenarios

  library(ggplot2)
  library(tidyverse)
  theme_set(theme_bw())
  sim4_full_results <- get("full_results",sim4_RStats_run_Full_simulation_results)
  full_results <- dplyr::left_join(param_grid,sim4_full_results)
  design_effect <- full_results %>% group_by(max_effect,response_type,Dose_Level) %>%
    reframe(neffect=length(unique(Expected_Response)), Reduction = paste0(100-Expected_Response[1],"%"))
  # ggplot(full_results%>% dplyr::filter(m_tanks==4, k_individuals==6, max_effect==5),aes(x=ICC,y=Power,color=Method))+ geom_point()+
  #   facet_grid(response_type ~ Dose_Level,scales = "free") +
  #   geom_line()+
  #   geom_hline(yintercept = c(0.05,0.8),lty=2,alpha=0.3)+
  #   theme(legend.position = "bottom")+ ggplot2::scale_color_viridis_d(direction = 1)

  for(max_effect0 in c(5,20)){
    if(max_effect0 ==5){
      tempdata <- full_results%>%dplyr::filter(response_type!="oscillating") %>% droplevels(.)
      design_effect_temp <- design_effect %>% dplyr::filter(response_type != "oscillating")
    }else{
      tempdata <- full_results%>%dplyr::filter(response_type!="none") %>% droplevels(.)
      design_effect_temp <- design_effect %>% dplyr::filter(response_type != "none")
    }
    for(m_tank0 in c(4,6)){
    for(k_individuals0 in c(3,6,10)){

      ggplot(tempdata%>%
               dplyr::filter(m_tanks==m_tank0, k_individuals==k_individuals0,
                             max_effect==max_effect0),aes(x=ICC,y=Power,color=Method))+ geom_point()+
        facet_grid(response_type ~ Dose_Level,scales = "free")  +
        geom_line()+
        geom_hline(yintercept = c(0.05,0.8),lty=2,alpha=0.3)+
        theme(legend.position = "bottom")+ ## ggplot2::scale_color_viridis_d(direction = 1)
        ggthemes::scale_color_solarized()+ggtitle(paste0("4 tanks, ", k_individuals0, " individuals, maximum effect: ", max_effect0, "%"))+
        geom_text(data=design_effect_temp%>%dplyr::filter(max_effect==max_effect0), aes(x=0.5,y=1.05,label=Reduction), size=2,col= "black" )
      ggsave(paste0("SimPower_",m_tank0, "_tank_",k_individuals0,"_ind_",max_effect0,"_effect.png"),dpi=300, width = 6,height =5)
    }

    }
  }






}
