## Dose-response data can be simulated via rdrm function or genDF function from MCPMod

library(MCPMod)

# use emax model
genDFdata("emax", c(e0 = 0.2, eMax = 1, ed50 = 0.05), c(0,0.05,0.2,0.6,1), 20, 2)
# use fixed mean vector
genDFdata(mu = 1:5, doses = 0:4, n = c(20, 20, 10, 5, 1), sigma = 0.2)


datsim1 <- genDFdata(mu = c(10,9,8,7,6), doses = 0:4, n = 10, sigma = 0.2) %>% rename(Dose=dose,Response = resp)
prelimPlot3(datsim1)
prelimPlot2(datsim1)
dat_shallow <- datsim1
usethis::use_data(dat_shallow,overwrite = TRUE)


dat_noED50 <- rdrm(1, LL.4(), c(`b:(Intercept)` = 3, `c:(Intercept)` = 6,
                             `d:(Intercept)` = 8, `e:(Intercept)` = 3),
               xerror=c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
                                                                                 3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
               yerror = "rnorm", ypar = c(0, 0.6))
dat_noED50 <- data.frame(Dose = dat_noED50$x[1,], Response = dat_noED50$y[1,])
prelimPlot2(dat_noED50)
usethis::use_data(dat_noED50,overwrite = TRUE)

mod1 <- drm(Response~Dose,data=dat_noED50,fct=LL.4())
## plot(mod1,type="all")
result <- calcSteepnessOverlap(mod = mod1, trend = "Decrease")
ED(mod1,50)
ED.plus(mod1, c(10, 20, 50), trend = "Decrease", CI = "inv")
