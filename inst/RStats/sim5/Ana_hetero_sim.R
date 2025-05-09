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
# gls_dunnett_agg_simple <- function(data, alpha = 0.05, alternative = "less") {
#   gls_dunnett_agg(data,alpha,alternative,notAgged =FALSE)
# }
#
# lm_dunnett_agg_simple <- function(data, alpha = 0.05, alternative = "less") {
#   lm_dunnett_agg(data,alpha,alternative,notAgged =FALSE)
# }


# Variance pattern generator functions
variance_patterns <- list(
  homogeneous = function(n_doses, base_var = 4) {
    rep(base_var, n_doses)
  },

  increasing = function(n_doses, base_var = 4) {
    base_var * seq(0.5, 2, length.out = n_doses)
  },

  decreasing = function(n_doses, base_var = 4) {
    base_var * seq(2, 0.5, length.out = n_doses)
  },

  v_shaped = function(n_doses, base_var = 4) {
    mid_point <- ceiling(n_doses/2)
    c(
      base_var * seq(2, 1, length.out = mid_point),
      base_var * seq(1, 2, length.out = n_doses - mid_point + 1)[-1]
    )
  }
)

tank_level_methods <- list(
  "LM_Homo" = lm_dunnett_agg_simple,
  "LM_Hetero" = gls_dunnett_agg_simple,
  "Williams" = williams_test,
  "Jonckheere" = jonckheere_test,
  "Dunns" = manyone_dunns_test
)

# Visualize the variance patterns
# variance_plot <- plot_variance_patterns(n_doses = 5, base_variance = 2)
# print(variance_plot)

# Parameter grid for heterogeneous variance question
param_grid_hetero <- expand.grid(
  n_doses = 5,
  min_dose = 0,
  max_dose = 20,
  m_tanks = c(4,6,10),
  variance_pattern = c("homogeneous", "increasing", "decreasing", "v_shaped"),
  base_variance = c(4, 6),
  max_effect = c(5, 20),  # Null and moderate effect
  response_type = c("none", "threshold", "oscillating"),  # Focus on null and decreasing
  alpha = 0.05,
  alternative = "less",
  stringsAsFactors = FALSE
)

nrow(param_grid_hetero)
## 120 ## 24*2*3
param_grid_hetero <- param_grid_hetero %>% dplyr::filter(max_effect == 5 | (max_effect == 20 & response_type !="none"))


# sim5_results <- run_multiple_hetero_simulations(param_grid=param_grid_hetero,
#                                                 test_methods=tank_level_methods,
#                                                 n_sim = 1000,
#                                                 n_cores = parallel::detectCores() - 1,
#                                                 seed = 123)
#
# ## sim5_results <- plyr::ldply(hetero_simulation_checkpoint$results_list)
#
# saveRDS(sim5_results,file="sim5_results.rds")
## Save the output into an R data object.

sim5_results <- readRDS("~/Projects/drcHelper/inst/RStats/sim5/sim5_results.rds")
sim5_results$Method[sim5_results$Method=="GLS_Hetero"] <- "LM_Hetero"
sim5_wide_res <- sim5_results %>% tidyr::pivot_wider(names_from = Method,values_from = Power)

sim5_wide_res %>% dplyr::filter(Expected_Response<100)%>%summarise(nHetero = sum(LM_Hetero>=LM_Homo),n0=length(LM_Homo)) %>% mutate(pnHetero=nHetero/n0)
sim5_wide_res %>% dplyr::filter(Expected_Response<100)%>% filter(LM_Hetero - LM_Homo>0)
sim5_wide_res %>% dplyr::filter(Expected_Response<100)%>% filter(LM_Hetero < LM_Homo) %>% filter(m_tanks ==6, max_effect==5,response_type =="threshold")
sim5_wide_res %>% dplyr::filter(Expected_Response<100)%>% mutate (pdiff= LM_Hetero - LM_Homo) ->tmp


sim5_wide_res %>% dplyr::filter(response_type=="none")%>% summarise(npJ1= sum(Jonckheere>0.1)/length(Jonckheere),
                                                                    npJ2= sum(Jonckheere>0.05)/length(Jonckheere),
                                                                    npW1= sum(Williams>0.1)/length(Jonckheere),
                                                                    npW2= sum(Williams>0.05)/length(Jonckheere),
                                                                    npHeter= sum(LM_Hetero>0.05)/length(Jonckheere),
                                                                    npHomo= sum(LM_Homo>0.05)/length(Jonckheere)
                                                                    )

sim5_wide_res %>% dplyr::filter(Expected_Response==100)%>% summarise(npJ1= sum(Jonckheere>0.1)/length(Jonckheere),
                                                                    npJ2= sum(Jonckheere>0.05)/length(Jonckheere),
                                                                    npW1= sum(Williams>0.1)/length(Jonckheere),
                                                                    npW2= sum(Williams>0.05)/length(Jonckheere),
                                                                    npHeter= sum(LM_Hetero>0.05)/length(Jonckheere),
                                                                    npHomo= sum(LM_Homo>0.05)/length(Jonckheere)
)



out <- sim5_wide_res %>% group_by(Expected_Response) %>% nest() %>% mutate(power_summary=map(data,function(x){
  out <- data.frame(stat=c("Min","Q1","Mean","Median","Q3","Max"))
  out1 <- x %>% reframe(LM_Homo = summary(LM_Homo),
                        LM_Hetero = summary(LM_Hetero),
                        Jonckheere = summary(Jonckheere),
                        Williams = summary(Williams),
                        Dunns = summary(Dunns)
  )
  cbind(out,out1)

})) %>% dplyr::select(-data) %>% unnest(c(power_summary))

out

########################################################################
plotit <- FALSE

if(plotit){
  # Plot results for different scenarios

  library(ggplot2)
  library(tidyverse)
  theme_set(theme_bw())
  ##
  sim5_results$Method[sim5_results$Method=="GLS_Hetero"] <- "LM_Hetero"
  sim5_results$Dose_Level <- gsub("Dose_","T",paste0(sim5_results$Dose_Level,"\n",(100-sim5_results$Expected_Response),"%"))
  design_effect <- sim5_results %>% group_by(max_effect,response_type,Dose_Level,variance_pattern,base_variance) %>%
    reframe(neffect=length(unique(Expected_Response)), Reduction = paste0(100-Expected_Response[1],"%"),
            nvar=length(unique(Variance)),Variance=Variance[1])
  # ggplot(sim5_results%>% dplyr::filter(m_tanks==4, k_individuals==6, max_effect==5),aes(x=ICC,y=Power,color=Method))+ geom_point()+
  #   facet_grid(response_type ~ Dose_Level,scales = "free") +
  #   geom_line()+
  #   geom_hline(yintercept = c(0.05,0.8),lty=2,alpha=0.3)+
  #   theme(legend.position = "bottom")+ ggplot2::scale_color_viridis_d(direction = 1)

########################################################
  tempdata <- sim5_results
  for(max_effect0 in c(5,20)){
    for(m_tank0 in c(4,6,10)){
      for(response_type0 in c("none","oscillating","threshold")){
        if(max_effect0 == 20 && response_type0 =="none"){}else{
          ggplot(tempdata%>%
                   dplyr::filter(m_tanks==m_tank0, response_type == response_type0,
                                 max_effect==max_effect0)%>%droplevels(.),
                 aes(x=Dose_Level,y=Power,color=Method))+
            geom_point(aes(pch=Method))+
            facet_grid( variance_pattern~base_variance,scales = "free")  +
            #geom_line()+
            geom_hline(yintercept = c(0.05,0.8),lty=2,alpha=0.3)+
            scale_y_continuous(breaks = seq(0, 1, by = 0.2))+
            theme(legend.position = "bottom")+ ## ggplot2::scale_color_viridis_d(direction = 1)
            ggthemes::scale_color_solarized()+ggtitle(paste0("response type: ", response_type0,", ", m_tank0, " tanks, ", "maximum effect: ", max_effect0, "%"))+
            geom_text(data=design_effect%>%dplyr::filter(max_effect==max_effect0,response_type == response_type0)%>%droplevels(.),
                      aes(x=Dose_Level,y=1.15,label=paste("Var=",Variance)), size=2.5,col= "black" )
          ggsave(paste0("SimHetero_",m_tank0, "_tank_","response_", response_type0,max_effect0,"_effect.png"),
                 dpi=300, width = 6,height = 6)

        }


      }
    }
  }






}

