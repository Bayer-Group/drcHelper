dat_bcs1 <- dat_bcs1%>% dplyr::select(-c(S0_1,S0_2)) %>% mutate(tmt = ifelse(tmt == "SC", "C", tmt))

res1 <- step_down_RSCABS(
  dat_bcs1,
  treatment_col = "tmt",replicate_col = "tank",
  treatment_order = c("C","T1","T2","T3"),
  alternative = "greater"
)
res1$combined_results
res1
plot(res1)
print(res1,printLowest = T)

dat1 <- convert_fish_data(dat_bcs1,direction="to_individual",treatment_col = "tmt",replicate_col = "tank" )
dat1 <- (dat1) %>% mutate(score1=as.numeric(factor(score))-1,score2=score1+5) %>% dplyr::select(-c(score)) %>% as.data.frame

## Note runRSCABS expects more than one endpoints.
res <- runRSCABS(dat1,'tmt','tank',test.type='RS')
res[1:3,] %>% mutate(Effect = gsub("score1","S",Effect) )
