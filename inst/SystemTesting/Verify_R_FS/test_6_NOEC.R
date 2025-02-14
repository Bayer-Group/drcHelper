## res <- testthat::test_file("inst/SystemTesting/Verify_R_FS/test_6_NOEC.R",reporter =  testthat::ListReporter)

res%>%
  as_tibble() %>%
  rename(Test = test) %>%
  group_by(file, context, Test) %>%
  ##tidyr::separate_wider_delim(Test, names = c("Context1", "Test1"), delim = ":")%>%
  summarise(NumTests = first(nb),
            Passed   = sum(passed),
            Failed   = sum(failed),
            Warnings = sum(warning),
            Errors   = sum(as.numeric(error)),
            Skipped  = sum(as.numeric(skipped)),
            .groups = "drop") %>%
  tidyr::separate_wider_delim(Test, names = c("Context", "Test"), delim = ":")%>%
  dplyr::select(-context)

describe("Tests to derive NOECs",{
  it("Fisher's Test",{

  })



  it("Dunn's-Test with Kruskal-Wallis pretest",{

    Rate=c(0,0,0,0,0,0,
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
    Response =data.frame(Rate, y)

    res <- kruskal.test(y ~ Rate, data=Response)
    expect_identical(res,structure(list(statistic = c(`Kruskal-Wallis chi-squared` = 27.3924731182796),
                                        parameter = c(df = 6L), p.value = 0.000122233004178165, method = "Kruskal-Wallis rank sum test",
                                        data.name = "y by Rate"), class = "htest"),tolerance=1e-4)
    library(DescTools)
    dunn_df <- DunnTest(y ~ Rate, data=Response, method="none", alternative ="greater",out.list = FALSE)
    dunn_df
    expect_snapshot()
    dunn_pval <- c()
    for (j in 1:nrow(dunn_df[[1]])) {
      dunn_pval <- c(dunn_pval, dunn_df[[1]][j, 1])
    }
    dunn_pval
    p.adjust(dunn_pval, method = "holm")
    PMCMRplus::kwManyOneDunnTest(y ~ Rate, data=Response)
    res2 <- PMCMRplus::kwManyOneDunnTest(y ~ Rate, data=Response, alternative = "less",p.adjust.method = "holm")
    expect_identical(as.numeric(res2$p.value),p.adjust(dunn_pval, method = "holm"))
    res1 <- DunnTest(y ~ Rate, data=Response, method="none", alternative ="greater")
    expect_identical(as.numeric(res2$p.value),p.adjust(as.numeric(res1$pmat[1,-1]), method = "holm"))
    ## Here we tested that both PMCMRplus::kwManyOneDunnTest and DunnTest from DescTools generated that same results.
  })


  it("Rank sum Test",{

  })

  it("Student's t-test",{
    y0<-c(0.120915680098082,0.136752501891969,0.145668808436602,0.131844763607024,0.149892124046466,0.133572261866272)
    y1<-c(0.131517109035102,0.117455425985384,0.130835155683102,0.12226548296818,0.127485057136569,0.128828137633933)

    res <- t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="less")
    expect_equal(res$p.value,0.03107260591156, tolerance = 1e-4)
    res <- t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="greater")
    expect_equal(res$p.value,0.9689273940884, tolerance = 1e-4)
    res <- t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="two.sided")
    expect_equal(res$p.value,0.06214521182311, tolerance = 1e-4)
    res <- t.test(y1,y0, paired=FALSE, alternative="two.sided")
    ## Testing also the default t-test (Welch's t-test)
    expect_equal(res$p.value, 0.07085, tolerance = 1e-4)
  })

  it("Welch's t-test",{
    y0<-c(0.131517109035102,0.117455425985384,0.130835155683102,0.12226548296818,0.127485057136569,0.128828137633933)
    y1<-c(0.122888192029009,0.126866725094641,0.128467082586674,0.116653888503673)
    y2<-c(0.0906079518188219,0.102060998252763,0.107240263636048,0.0998663441976353)
    y3<-c(0.0584507373938537,0.066439126181113,0.0806046608441279,0.0828404794158172)
    y4<-c(0.0462632004630849,0.0461876546375037,0.0512317665813575,0.0416533060961155)
    y5<-c(0.0267638178172436,0.0314508456741666,0.0237960318948956,0.0295133681572911)
    y6<-c(0.0273565894468647,0.0324226779638651,0.0289617934362975,0.0305317153447814)
    res1 <- t.test(y1, y0, var.equal=FALSE, paired=FALSE, alternative="less")
    expect_equal(res1$p.value, 0.2320348713658, tolerance = 1e-4)

    res2 <- t.test(y2, y0, var.equal=FALSE, paired=FALSE, alternative="less")
    expect_equal(res2$p.value, 0.0005013, tolerance = 1e-4)

    dat <- data.frame(y=c(y0,y1,y2,y3,y4,y5,y6), Dose=factor(c(rep("Control",6),rep(1:6,each=4)),levels=c("Control",1:6)))
    mod <- lm(y~Dose,data=dat)


  })
  it("Wilcoxon Test",{

  })



  it("Welch's t-test",{

  })


  it("William's Test",{
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


  it("Dunnett Test",{

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
    set.seed(100) ## if not setting random seed, snapshot won't work as the mvt would change.
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
    expect_snapshot(glht(mod1,linfct = mcp(Dose="Dunnett")))
    expect_snapshot(glht(mod1,linfct = mcp(Dose="Dunnett"),alternative="less"))
  })

  it("Two Sample t-test",{
    y0<-c(0.120915680098082,0.136752501891969,0.145668808436602,0.131844763607024,0.149892124046466,0.133572261866272)
    y1<-c(0.131517109035102,0.117455425985384,0.130835155683102,0.12226548296818,0.127485057136569,0.128828137633933)

    res <- t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="less")
    t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="greater")
    t.test(y1,y0, var.equal=TRUE, paired=FALSE, alternative="two.sided")
  })

  it("Jonckheere Terpstra Test",{
    ## DescTools::JonckheereTerpstraTest()


  })


})

