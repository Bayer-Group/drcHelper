## testthat::test_file("inst/SystemTesting/Verify_R_FS/test_5_summary.R",reporter =  testthat::ListReporter)
# res %>% as_tibble() %>%
#   rename(Test = test) %>%
#   group_by(file, context, Test) %>%
#   summarise(NumTests = first(nb),
#             Passed   = sum(passed),
#             Failed   = sum(failed),
#             Warnings = sum(warning),
#             Errors   = sum(as.numeric(error)),
#             Skipped  = sum(as.numeric(skipped)))
test_that("Mean and other summary statistics can be calculated correctly",{
  Rate = c(0,0,0,0,0,0,
           0.0448,0.0448,0.0448,0.0448,
           0.132,0.132,0.132,0.132,
           0.390,0.390,0.390,0.390,
           1.15,1.15,1.15,1.15,
           3.39,3.39,3.39,3.39,
           10.0,10.0,10.0,10.0)
  y=c(40.6666666666667,31.3333333333333,39.3333333333333,39.3333333333333,39.6666666666667,35.5,
      35.1666666666667,35.1666666666667,41.1666666666667,34.3333333333333,
      15.3333333333333,24.3333333333333,23.8333333333333,21.3333333333333,
      9.5,11,11.5,13.5,
      6.83333333333333,6.66666666666667,7.16666666666667,6.33333333333333,
      3.33333333333333,4.33333333333333,2.83333333333333,3.66666666666667,
      3.5,4.5,3.66666666666667,4)
  Response = data.frame(Rate, y)

  library(dplyr)
  ## This is the degree of freedom
  df_num <- nrow(Response) - nrow(Response %>% group_by(Rate) %>% summarise(n = n()))

  #Basis-Statistik 1. Teil
  tmp_data <- Response %>%
    group_by(Rate) %>%
    summarise(n = n(), mean=mean(y), var=var(y),  std.dev=sd(y), W=n/var)

  n_control <- tmp_data[ (tmp_data$Rate == 0), ]$n
  mean_control <- tmp_data[ (tmp_data$Rate == 0), ]$mean
  W_control <- tmp_data[ (tmp_data$Rate == 0), ]$W

  #Basis-Statistik 2. Teil
  ## Note that Reduction is just negative Inhibition, no need to recalculate.
  ## How variance of mdd is calculated
  ## vgl = Versuchsglieder
  tmp_data <- tmp_data %>%
    mutate( CV=std.dev/mean*100, Inhibition = ifelse(Rate == 0, NA, (1-mean/mean_control)*100),
             Reduction = ifelse(Rate == 0, NA, (mean/mean_control-1)*100),
             # df = ifelse(Rate == 0, 2*(n-1), 1/((1-(W_control/W_vgl))^2/(n_control-1)+(1-W/W_vgl)^2/(n-1))   ),
             var_mdd = var*(n-1)
            )

  #RMSE über alle Dosis-Werte berechnen
  tmp_var <- tmp_data %>%
    summarise(sum_var_mdd=sum(var_mdd))
  RMSE <- sqrt(tmp_var$sum_var_mdd/df_num)
  tmp <- summary(mod)
  mod <- lm(y~factor(Rate),data=Response)
  expect_equal(sqrt(sum((residuals(mod))^2)/23),RMSE,info="test if RMSE can be calculated correctly")
  expect_equal(tmp$sigma,RMSE, info = "RMSE is also an estimation for sigma")
  #se für jedes Treatment berechnen (später nötig für MDD-Berechnung)
  ## Note that this se is really due to the two-sample t-test, $t_i = \frac{\bar x_i - \bar x_0}{RMSE*\sqrt{1/n_i+1/n_0}}$
  tmp_data <- tmp_data %>%
    mutate( se_mdd = ifelse(Rate == 0, NA, RMSE*sqrt(1/n+1/n_control)   ) )

  expect_equal(tmp$df[2], df_num,info="degree of freedom is calculated")
  expect_snapshot(tmp_data)

})
