#library(testthat)
## testing functions in R/Endpoints.R

describe("williamsTest",{
  it("williamsTest in PMCMRplus returns correct data frame structure",{
    require(PMCMRplus)
    x <- c(106, 114, 116, 127, 145,
           110, 125, 143, 148, 151,
           136, 139, 149, 160, 174)
    g <- gl(3, 5)
    levels(g) <- c("0", "I", "II")

    # Perform Williams Test
    res <- williamsTest(x ~ g)
    expect_identical(res,structure(list(method = "Williams trend test", data.name = "x by g",
                                        crit.value = structure(c(1.78228755564932, 1.873), dim = 2:1, dimnames = list(
                                          c("mu1", "mu2"), "ctr")), statistic = structure(c(1.35677190561512,
                                                                                            2.94950414264156), dim = 2:1, dimnames = list(c("mu1", "mu2"
                                                                                            ), "ctr")), parameter = c(df = 12L), alternative = "greater",
                                        dist = "t'"), class = "osrt"),tolerance =1e-3)

  })
  it("summaryZG returns correct data frame structure",{
    # Call summaryZG
    result <- summaryZG(res)
    # Check if the result is a data frame, this function is an intermediate step for the next function
    expect_true(is.data.frame(result))
  })
  it("summaryZG returns correct data frame structure",{
    tmp <- getwilliamRes(res)
    expect_identical(tmp,c("accept", "reject"))
  })
})

describe("getEndpoint()",{
  it("stepDown procedure can get endpoint correctly",{
    pvals <- c(0.01, 0.03, 0.07, 0.08)
    doses <- c("Control","A", "B", "C", "D")
    expect_equal(getEndpoint(pvals, doses),"D")
    pvals <- c(0.01, 0.03, 0.02, 0.04)
    expect_equal(getEndpoint(pvals, doses),"Control")
    pvals <- c(0.01, 0.03, 0.06, 0.04)
    expect_equal(getEndpoint(pvals, doses),"C")
    pvals <- c(0.05, 0.06, 0.04, 0.09)
    expect_equal(getEndpoint(pvals,doses,procedure = "stepUp"),"B")

  })
  it("stepUp procedure works",{
    expect_equal(getEndpoint(pvals,doses,procedure = "stepDown"),"D")
  })
})

describe("pavaMean()",{
  set.seed(123)
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  g <- factor(rep(c("A", "B", "C"), each = 3))

  # Unit tests
  it("pavaMean calculates correctly", {
    result <- pavaMean(x, g)

    expect_s3_class(result, "data.frame")
    expect_named(result, c("pavaMean", "SE.diff"))
    expect_equal(nrow(result), length(levels(g)))  # Number of rows should equal number of groups
  })

  it("pavaMean handles less alternative", {
    result <- pavaMean(x, g, alternative = "less")

    expect_true(all(result$pavaMean <= max(x)))  # Adjusted means should be less than or equal to max
  })

  it("pavaMean stops for too many groups", {
    expect_error(pavaMean(x=1:15, factor(rep(1:15, each = 1))),
                 "Critical t-values are only available for up to 10 dose levels.")
  })

  it("pavaMean handles NA values correctly", {
    x_na <- c(1, 2, NA, 4, 5, 6, 7, 8, 9)
    result <- pavaMean(x_na, g)

    expect_equal(nrow(result), length(levels(g)))  # Number of rows should equal number of groups
  })
})
