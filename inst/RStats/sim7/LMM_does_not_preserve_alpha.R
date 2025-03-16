ICC <- 0.3

total_variance <- 10

# Function to calculate variance components from ICC
calculate_variance_components <- function(icc, total_variance) {
  var_tank <- icc * total_variance
  var_individual <- (1 - icc) * total_variance
  return(list(var_tank = var_tank, var_individual = var_individual))
}

calculate_variance_components(0.3,10)
var_tank  <- 3
var_individual <- 7

## no dose response.
response_fn <- function(dose, max_effect) {
  rep(100, length(dose))
}
# Generate data
sim_data <- simulate_dose_response(
  n_doses = 5,
  dose_range = c(0,20),
  m_tanks = 4,
  k_individuals = 6,
  var_tank = var_tank,
  var_individual = var_individual,
  include_individuals = TRUE,
  response_function = function(dose) response_fn(dose, max_effect)
)
drcHelper::prelimPlot3(sim_data)

# Plot individual data points with tank means
sim_data <- example_type1error_data
ggplot(sim_data, aes(x = factor(Dose), y = Response, color = factor(Tank))) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = factor(Tank))) +
  labs(title = "Simulated data with 'none' response pattern \nbut with significance identified at dose level of 5",
       x = "Dose", y = "Response",
       color = "Tank") +
  theme_bw()+theme(legend.position = "bottom")+ggthemes::scale_color_solarized()
ggsave("~/Projects/drcHelper/inst/RStats/sim7/LMM_typeIerror.png",dpi=300, width = 6,height =5)

result <- lmm_dunnett_homo(sim_data)
saveRDS(sim_data,"example_type1error_data.rds")
if(any(result$significant)) print("stop")
