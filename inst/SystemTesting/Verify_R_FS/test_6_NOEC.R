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
    ## note that the 1st run has been checked against the developer's document manually.
    res1
    expect_snapshot(williamsTest_JG(df= Response, trt='Rate', resp='y', direction='decreasing'))
    expect_snapshot(williamsTest_JG(df= Response, trt='Rate', resp='y', direction='increasing'))

    res2 <- PMCMRplus::williamsTest(y~Rate,data=Response,alternative="less")
    res2
    expect_snapshot(PMCMRplus::williamsTest(y~Rate,data=Response,alternative="less"))
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
    expect_snapshot(DunnettTest(y ~ Rate, data = Response))
    expect_snapshot(PMCMRplus::dunnettTest(y ~ Rate,alternative="less"))
    expect_snapshot(PMCMRplus::dunnettTest(y ~ Rate,alternative="greater"))
    expect_snapshot(PMCMRplus::dunnettTest(y ~ Rate,alternative="two.sided"))
    Response <- Response %>% mutate(Dose = factor(Rate))
    mod1 <- lm(y~Dose,data=Response)
    library(multcomp)
    res <- glht(mod1,linfct = mcp(Dose="Dunnett"))
    res <- glht(mod1,linfct = mcp(Dose="Dunnett"),alternative="less")
    summary(res)
    summary(res)
    expect_snapshot(glht(mod1,linfct = mcp(Dose="Dunnett")))
    expect_snapshot(glht(mod1,linfct = mcp(Dose="Dunnett"),alternative="less"))
  })

  test_that("Two Sample t-test",{
    y0<-c(0.120915680098082,0.136752501891969,0.145668808436602,0.131844763607024,0.149892124046466,0.133572261866272)
    y1<-c(0.131517109035102,0.117455425985384,0.130835155683102,0.12226548296818,0.127485057136569,0.128828137633933)

    t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="less")

    t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="greater")
    t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="two.sided")
  })

  test_that("Jonckheere Terpstra Test",{
    ## DescTools::JonckheereTerpstraTest()


  })


})

