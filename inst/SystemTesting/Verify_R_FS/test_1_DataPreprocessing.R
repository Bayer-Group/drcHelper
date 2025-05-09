test_that("NTA mortality can be calculated from the alive, dead, total columns", {
  product <- c("Control", "Control", "Control", "Control", "Control", "Control", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item","Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item","Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Test item", "Reference item", "Reference item", "Reference item", "Reference item", "Reference item", "Reference item")

  dose <- c(0, 0, 0, 0, 0, 0, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.375, 0.375, 0.375, 0.375, 0.375, 0.375, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 2, 2, 2, 2, 2, 2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

  alive <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 5, 5, 4, 5, 4, 4, 5, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)

  dead  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4)

  total <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)

  NTA_Ar_ext_mortality = data.frame(product, dose, alive, dead, total)

  library(dplyr)
  NTA_Ar_ext_mortality <- NTA_Ar_ext_mortality %>%
    group_by(product, dose) %>%
    summarize(alive = sum(alive), dead = sum(dead), total = sum(total))

  NTA_Ar_ext_mortality <- NTA_Ar_ext_mortality %>%
    mutate( unCorrectedMortality = 100 * dead / total
            , )

  control_unCorrectedMortality <- NTA_Ar_ext_mortality[(NTA_Ar_ext_mortality$dose == 0),]$unCorrectedMortality

  NTA_Ar_ext_mortality <- NTA_Ar_ext_mortality %>%
    mutate( CorrectedMortality = (unCorrectedMortality - control_unCorrectedMortality) / (1 - control_unCorrectedMortality / 100))


  expect_identical(NTA_Ar_ext_mortality,NTA_Ar_ext_mortality_expected)
  # Add more assertions as needed
})

test_that("NTP SE_DE emergence and survival can be calculated correctly",{

  rate = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,0.41,
           1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,1.02,
           2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,2.56,
           6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,6.4,
           16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
           40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,
           120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120,120)

  total = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

  #values of column alive for endpoint = “Seedling emergence”
  em_alive = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
               2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,
               2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,1,
               2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,1,
               1,1,2,2,2,2,1,1,1,2,2,1,2,2,2,1,1,2,2,1,
               1,1,2,1,2,2,1,1,1,2,2,1,2,2,2,1,1,1,1,1,
               1,1,1,1,2,2,1,1,1,1,1,1,1,2,2,1,1,1,1,1,
               1,1,1,1,1,1,0,1,1,1,1,1,1,2,0,1,1,0,1,1)

  #values of column alive for endpoint = “Survival of emerged seedlings”
  sur_alive = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,
                2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,1,
                2,1,1,2,1,2,2,2,2,2,2,1,2,1,2,1,1,2,2,0,
                2,1,1,2,1,2,1,2,2,1,2,0,1,1,1,1,1,2,1,1,
                1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,0,
                1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,0,
                1,0,1,0,1,1,1,1,0,1,0,1,0,1,1,0,1,0,0,0,
                1,0,1,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0)

  NTP_example = data.frame(rate, total, em_alive, sur_alive)

  NTP_example <- NTP_example %>%
    mutate(
      #If only one seedling sprouted, and this seedling also survived, the value is corrected
      sur_alive_corr = ifelse(em_alive == 0, 0, sur_alive/em_alive * total)
      , #Uncorrected reduction for Emergence
      em_reduction_uncorr = (1 - em_alive / total) * 100
      , #Uncorrected reduction for Survival
      sur_reduction_uncorr = (1 - sur_alive / em_alive) * 100
      , )

  mean_control_em_reduction_uncorr <- mean(NTP_example[ (NTP_example$rate == 0), ]$em_reduction_uncorr)
  mean_control_sur_reduction_uncorr <- mean(NTP_example[ (NTP_example$rate == 0), ]$sur_reduction_uncorr)

  NTP_example <- NTP_example %>%
    mutate( #Corrected reduction for Emergence
      em_reduction_corr = (em_reduction_uncorr - mean_control_em_reduction_uncorr) / (1 - mean_control_em_reduction_uncorr),
      #Corrected reduction for Survival
      sur_reduction_corr = 100 - (100 - sur_reduction_uncorr) / (100 - mean_control_sur_reduction_uncorr) * 100,
      #Normalized Emergence (based on corrected reduction)
      em_norm = ifelse(em_reduction_corr < 0, 0, ifelse(em_reduction_corr > 100, 1, em_reduction_corr/100)),
      #Normalized Survival (based on corrected reduction)
      sur_norm = ifelse(sur_reduction_corr < 0, 0, ifelse(sur_reduction_corr > 100, 1, sur_reduction_corr/100))
      )

  NTP_example_rate <- NTP_example %>%
    group_by(rate) %>%
    summarize( sum_total = sum(total)
               , sum_em_alive = sum(em_alive)
               , sum_sur_alive_corr = sum(sur_alive_corr)

               , em_reduction_uncorr_mean = mean(em_reduction_uncorr)
               , sur_reduction_uncorr_mean = mean(sur_reduction_uncorr, na.rm = TRUE)

               , em_reduction_corr_mean = mean(em_reduction_corr)
               , sur_reduction_corr_mean = mean(sur_reduction_corr, na.rm = TRUE)
    )
  expect_identical(NTP_example_rate,NTP_example_rate_expected)

})
