## res <- testthat::test_file("inst/SystemTesting/Verify_R_FS/test_6_NOEC.R",reporter =  testthat::ListReporter)

test_that("Tests to derive NOECs",{
  test_that("Fisher's Test",{

  })



  test_that("Dunn's-Test with Kruskal-Wallis pretest",{

  })


  test_that("Rank sum Test",{

  })

  test_that("Test",{

  })


  test_that("Wilcoxon Test",{

  })



  test_that("Welch's t-test",{

  })


  test_that("William's Test",{
    Rate = c(0,0,0,0,0,0,
             0.0448,0.0448,0.0448,0.0448,
             0.132,0.132,0.132,0.132,
             0.390,0.390,0.390,0.390,
             1.15,1.15,1.15,1.15,
             3.39,3.39,3.39,3.39,
             10.0,10.0,10.0,10.0)
    y=c(0.131517109035102,0.117455425985384,0.130835155683102,0.12226548296818,0.127485057136569,0.128828137633933,
        0.122888192029009,0.126866725094641,0.128467082586674,0.116653888503673,
        0.0906079518188219,0.102060998252763,0.107240263636048,0.0998663441976353,
        0.0584507373938537,0.066439126181113,0.0806046608441279,0.0828404794158172,
        0.0462632004630849,0.0461876546375037,0.0512317665813575,0.0416533060961155,
        0.0267638178172436,0.0314508456741666,0.0237960318948956,0.0295133681572911,
        0.0273565894468647,0.0324226779638651,0.0289617934362975,0.0305317153447814)
    Response = data.frame(Rate, y)

    res1 <- williamsTest_JG(df= Response, trt='Rate', resp='y', direction='decreasing')
    expect_snapshot(williamsTest_JG(df= Response, trt='Rate', resp='y', direction='decreasing'))
    expect_snapshot(williamsTest_JG(df= Response, trt='Rate', resp='y', direction='increasing'))



  })


  test_that("Dunnett Test",{

    Rate = c(0,0,0,0,0,0,
             0.0448,0.0448,0.0448,0.0448,
             0.132,0.132,0.132,0.132,
             0.390,0.390,0.390,0.390,
             1.15,1.15,1.15,1.15,
             3.39,3.39,3.39,3.39,
             10.0,10.0,10.0,10.0)
    y=c(0.131517109035102,0.117455425985384,0.130835155683102,0.12226548296818,0.127485057136569,0.128828137633933,
        0.122888192029009,0.126866725094641,0.128467082586674,0.116653888503673,
        0.0906079518188219,0.102060998252763,0.107240263636048,0.0998663441976353,
        0.0584507373938537,0.066439126181113,0.0806046608441279,0.0828404794158172,
        0.0462632004630849,0.0461876546375037,0.0512317665813575,0.0416533060961155,
        0.0267638178172436,0.0314508456741666,0.0237960318948956,0.0295133681572911,
        0.0273565894468647,0.0324226779638651,0.0289617934362975,0.0305317153447814)
    Response = data.frame(Rate, y)

    library("DescTools")
    DunnettTest(y ~ Rate, data = Response)
    PMCMRplus::dunnettTest(y ~ Rate,alternative="less")
    PMCMRplus::dunnettTest(y ~ Rate,alternative="greater")
    PMCMRplus::dunnettTest(y ~ Rate,alternative="two.sided")
  })



  test_that("Jonckheere Terpstra Test",{
    ## DescTools::JonckheereTerpstraTest()
  })


})

