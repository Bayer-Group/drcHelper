## Original Settings

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


sim4_full_results <- readRDS("~/Projects/drcHelper/inst/RStats/sim4/sim4_full_results.rds")

full_results <- dplyr::left_join(param_grid,sim4_full_results)

design_effect <- full_results %>% group_by(max_effect,response_type,Dose_Level) %>%
  reframe(neffect=length(unique(Expected_Response)), Reduction = paste0(100-Expected_Response[1],"%"))

wide_res <- full_results %>% tidyr::pivot_wider(names_from = Method,values_from = Power)

wide_res %>% filter(Expected_Response ==100) %>% reframe(LMM_Homo=summary(LMM_Homo),
                                                         LMM_Hetero=summary(LMM_Hetero),
                                                         LM_Homo = summary(LM_Homo),
                                                         LM_Hetero = summary(LM_Hetero),
                                                         Jonckheere = summary(Jonckheere),
                                                         Williams = summary(Williams),
                                                         Dunns = summary(Dunns)
                                                         ) %>% pander::pander(.)


wide_res %>% filter(Expected_Response ==95) %>% reframe(LMM_Homo=summary(LMM_Homo),
                                                         LMM_Hetero=summary(LMM_Hetero),
                                                         LM_Homo = summary(LM_Homo),
                                                         LM_Hetero = summary(LM_Hetero),
                                                         Jonckheere = summary(Jonckheere),
                                                         Williams = summary(Williams)
)

wide_res %>% filter(Expected_Response ==90) %>% reframe(LMM_Homo=summary(LMM_Homo),
                                                        LMM_Hetero=summary(LMM_Hetero),
                                                        LM_Homo = summary(LM_Homo),
                                                        LM_Hetero = summary(LM_Hetero),
                                                        Jonckheere = summary(Jonckheere),
                                                        Williams = summary(Williams)
)




out <- wide_res %>% group_by(Expected_Response) %>% nest() %>% mutate(power_summary=map(data,function(x){
  out <- data.frame(stat=c("Min","Q1","Mean","Median","Q3","Max"))
  out1 <- x %>% reframe(LMM_Homo=summary(LMM_Homo),
                LMM_Hetero=summary(LMM_Hetero),
                LM_Homo = summary(LM_Homo),
                LM_Hetero = summary(LM_Hetero),
                Jonckheere = summary(Jonckheere),
                Williams = summary(Williams)
  )
  cbind(out,out1)

})) %>% dplyr::select(-data) %>% unnest(c(power_summary))



out %>% mutate(across(is.table,
                      as.numeric))

out %>% mutate(across(is.table,
                      as.numeric)) %>% knitr::kable(.,digits = 3) %>% kableExtra::kable_classic()

pander::panderOptions("table.split.table" , Inf)
out %>% dplyr::filter(Expected_Response == 100)%>% pander::pander()

out  |> gt::gt() |>
  gt::fmt_number(
    columns = LMM_Homo,
    decimals = 3,
    use_seps = FALSE
  )



out2 <- wide_res %>% filter(m_tanks ==4, k_individuals ==6, max_effect == 5) %>% dplyr::select(-c(n_doses,min_dose,max_dose,
                                                                                                  m_tanks,k_individuals,alpha,
                                                                                                  alternative,
                                                                                                  max_effect,
                                                                                                  total_variance))
out2 %>% filter(Expected_Response <100) %>% summarise(nJ=sum(Jonckheere>Williams),n=length(Jonckheere))%>% mutate(pnJ = nJ/n)
out2 %>% pander::pander()

out2 %>% filter(response_type =="non_monotonic", Dose_Level == "Dose_4")

out1 <- wide_res %>% filter(m_tanks ==4, k_individuals ==6, max_effect == 20) %>% dplyr::select(-c(n_doses,min_dose,max_dose,
                                                                                                  m_tanks,k_individuals,alpha,
                                                                                                  alternative,
                                                                                                  max_effect,
                                                                                                  total_variance))

out1 %>% filter(response_type =="non_monotonic", Dose_Level == "Dose_4")
out1 %>% filter(Expected_Response <100) %>% summarise(nJ=sum(Jonckheere>Williams),n=length(Jonckheere))%>% mutate(pnJ = nJ/n)
out1 %>% filter(response_type =="oscillating", Dose_Level == "Dose_4")
out1 %>% filter(Dose_Level == "Dose_3") %>% group_by(response_type) %>% summarise(a=mean(Jonckheere))
