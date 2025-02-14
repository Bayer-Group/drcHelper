## test StatCharrms package

library(testthat)
describe("Williams test modified based on StatCharrms package works",{

  # Mock data for testing
  set.seed(123)
  mock_data <- data.frame(
    response_variable = c(rnorm(10, mean = 10), rnorm(10, mean = 12), rnorm(10, mean = 15)),
    Treatment = factor(rep(c("Control", "Dose1", "Dose2"), each = 10))
  )

  # Unit tests
  it("williamsTest_JG calculates correctly", {
    result <- williamsTest_JG(mock_data, "response_variable", "Treatment", "increasing")

    expect_s3_class(result, "data.frame")
    expect_named(result, c("Treatment", "Y.Tilde", "Y0", "Se.Diff", "DF", "Will", "TCrit", "Signif"))
    expect_true(all(result$Y.Tilde >= 0))  # Check for non-negative Y.Tilde values
    expect_identical(result,
                     structure(list(Treatment = c("Dose2", "Dose1"), Y.Tilde = c(14.58,
                                                                                 12.21), Y0 = c(10.0746, 10.0746), Se.Diff = c(0.4362, 0.4362),
                                    DF = c(27L, 27L), Will = c(10.32, 4.893), TCrit = c(1.783,
                                                                                        1.703), Signif = c("*", "*")), row.names = 2:1, class = "data.frame"),
                     tolerance = 1e-4)
  })

  it("williamsTest_JG handles invalid direction", {
    expect_error(williamsTest_JG(mock_data, "response_variable", "Treatment", "invalid_direction"),
                 "Please enter either 'decreasing' or 'increasing' for the direction.")
  })

  it("williamsTest_JG handles missing data", {
    mock_data_na <- mock_data
    mock_data_na$response_variable[c(1, 5)] <- NA  # Introduce NA values
    result <- williamsTest_JG(mock_data_na, "response_variable", "Treatment", "decreasing")

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) < nrow(mock_data))  # Check that some rows were removed
  })

  it("williamsTest_JG handles insufficient degrees of freedom", {
    small_data <- data.frame(
      response_variable = c(1, 2, 3),
      Treatment = factor(c("Control", "Dose1", "Dose2"))
    )

    expect_error(williamsTest_JG(small_data, "response_variable", "Treatment", "increasing"),
                 "Error: Williams test is not appropriate when DF < 5.")
  })

})

describe("WilliamsTest_JG gives the same results as in PCPMRplus::williamsTest",{
  it("Using Sachs data",{
    ## Example from Sachs (1997, p. 402)
    x <- c(106, 114, 116, 127, 145,
           110, 125, 143, 148, 151,
           136, 139, 149, 160, 174)
    g <- gl(3,5)
    levels(g) <- c("0", "I", "II")

    ## Williams Test
    res1 <- williamsTest(x ~ g)
    res2 <- williamsTest_JG(data.frame(Trt=x,Dose=g),"Trt", "Dose", "increasing")
    expect_equal(as.numeric(res1$crit.value),
                 res2$TCrit[rev(1:length(res2$TCrit))],tolerance = 1e-3)
    expect_equal(rev(as.numeric(res1$statistic)),res2$Will,tolerance = 1e-3)

  })
  it("pava Mean calculated correctly",{
    res3 <- pavaMean(x,g)
    expect_identical(rev(res3$pavaMean)[1:2],res2$Y.Tilde)
    expect_identical(rev(res3$SE.diff)[1:2],res2$Se.Diff,tolerance=1e-2)
  })
})
