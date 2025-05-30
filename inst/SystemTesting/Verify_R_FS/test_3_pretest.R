# testthat::test_file("inst/SystemTesting/Verify_R_FS/test_DataPreprocessing.R")
context("Testing Shapiro-Wilk normality test")

library(testthat)

test_that("Shapiro-Wilk test correctly assesses normality", {
  # Simulate some data
  set.seed(123)  # Set seed for reproducibility
  data <- rnorm(100)  # Generate random normal data

  # Perform Shapiro-Wilk test
  result <- shapiro.test(data)

  # Add your assertions here based on the expected behavior of the shapiro.test function
  expect_true(result$p.value > 0.05,
              info = "p-value is greater than 0.05, indicating normality")
  # Add more assertions as needed
})


test_that("Shapiro-Wilk test generates expected results",{
  vgl_0=c(0.131517109035102,0.117455425985384,0.130835155683102,0.12226548296818,0.127485057136569,0.128828137633933)
  vgl_1=c(0.122888192029009,0.126866725094641,0.128467082586674,0.116653888503673)
  vgl_2=c(0.0906079518188219,0.102060998252763,0.107240263636048,0.0998663441976353)
  vgl_3=c(0.0584507373938537,0.066439126181113,0.0806046608441279,0.0828404794158172)
  vgl_4=c(0.0462632004630849,0.0461876546375037,0.0512317665813575,0.0416533060961155)
  vgl_5=c(0.0267638178172436,0.0314508456741666,0.0237960318948956,0.0295133681572911)
  vgl_6=c(0.0273565894468647,0.0324226779638651,0.0289617934362975,0.0305317153447814)

  x0=vgl_0-mean(vgl_0)
  x1=vgl_1-mean(vgl_1)
  x2=vgl_2-mean(vgl_2)
  x3=vgl_3-mean(vgl_3)
  x4=vgl_4-mean(vgl_4)
  x5=vgl_5-mean(vgl_5)
  x6=vgl_6-mean(vgl_6)
  res <- shapiro.test(c(x0,x1,x2,x3,x4,x5,x6))
  expect_equal(as.numeric(res$statistic),0.9809809,tolerance = 1e-4)
  expect_equal(res$p.value,0.851,tolerance = 1e-4)

})


# test_levene_test.R
context("Testing Levene's test for equality of variances")

library(testthat)
library(lawstat)

test_that("Levene's test correctly assesses equality of variances", {
  # Simulate some data
  options(digits=16)
  library(lawstat)

  vgl_0=c(0.131517109035102, 0.117455425985384, 0.130835155683102, 0.12226548296818,
          0.127485057136569, 0.128828137633933)
  vgl_1=c(0.122888192029009, 0.126866725094641, 0.128467082586674, 0.116653888503673)
  vgl_2=c(0.0906079518188219, 0.102060998252763, 0.107240263636048, 0.0998663441976353)
  vgl_3=c(0.0584507373938537, 0.066439126181113, 0.0806046608441279, 0.0828404794158172)
  vgl_4=c(0.0462632004630849, 0.0461876546375037, 0.0512317665813575, 0.0416533060961155)
  vgl_5=c(0.0267638178172436, 0.0314508456741666, 0.0237960318948956, 0.0295133681572911)
  vgl_6=c(0.0273565894468647, 0.0324226779638651, 0.0289617934362975, 0.0305317153447814)

  x0=vgl_0-mean(vgl_0)
  x1=vgl_1-mean(vgl_1)
  x2=vgl_2-mean(vgl_2)
  x3=vgl_3-mean(vgl_3)
  x4=vgl_4-mean(vgl_4)
  x5=vgl_5-mean(vgl_5)
  x6=vgl_6-mean(vgl_6)
  library(lawstat)
  result <- lawstat::levene.test( c(x0, x1, x2,x3,x4,x5,x6),
                        factor(c(rep("x0",length(x0)),rep("x1",length(x1)),
                                 rep("x2",length(x2)), rep("x3",length(x3)), rep("x4",length(x4)),
                                 rep("x5",length(x5)), rep("x6",length(x6)))),location="mean", correction.method="none")

  # Add your assertions here based on the expected behavior of the Levene's test
  expect_true(format(result$p.value,digits=4) == "0.01148",
              info = "Levene test in the lawstat package: p-value is 0.1148, smaller than 0.05, indicating equality of variances")
  result <- car::leveneTest( c(x0, x1, x2,x3,x4,x5,x6),
                             factor(c(rep("x0",length(x0)),rep("x1",length(x1)),
                                      rep("x2",length(x2)), rep("x3",length(x3)), rep("x4",length(x4)),
                                      rep("x5",length(x5)), rep("x6",length(x6)))),center="mean")

  # Add your assertions here based on the expected behavior of the Levene's test
  expect_true(format(result$`Pr(>F)`[1],digits=4) == "0.01148",
              info = "Levene test in the car package with option of center being mean produces results the same as
              in the lawstat package: p-value is 0.1148, smaller than 0.05, indicating inequality of variances")

  result <- result <- car::leveneTest( c(x0, x1, x2,x3,x4,x5,x6),
                                       factor(c(rep("x0",length(x0)),rep("x1",length(x1)),
                                                rep("x2",length(x2)), rep("x3",length(x3)), rep("x4",length(x4)),
                                                rep("x5",length(x5)), rep("x6",length(x6)))))
  expect_true(format(result$`Pr(>F)`[1],digits=4) == "0.03616",
              info = "Levene test in the car package with default setting of center being median produces
              different p-values compared with that with center being mean, but still indicating inequality of variances")

})



