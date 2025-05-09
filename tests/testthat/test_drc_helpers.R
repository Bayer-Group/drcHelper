## testthat::test_file("tests/testthat/test_drc_helpers.R")
describe("calcSteepnessOverlap", {
  datTn<- subset(oecd201,Time==72)
  mod <- drm(Yield~Concentration,data=datTn,fct=LL.3())
  it("calculates steepness and overlap correctly for valid model", {
    result <- calcSteepnessOverlap(mod = mod, trend = "Decrease")
    expect_type(result, "character")
    expect_length(result, 2)
    expect_equal(result,c("High","Medium"))


  })

  it("handles missing EC50 values gracefully", {
    ## need a dataset with no effects greater than 50%.
    mod1 <- drm(Response~Dose,data=dat_noED50,fct=LL.4())
    ## plot(mod1,type="all")
    ## suppress unnecessary warnings
    suppressWarnings(result <- calcSteepnessOverlap(mod = mod1, trend = "Decrease"))

    expect_equal(result[2], "Not Defined")
  })

  it("returns NA when both mod and obj are NULL", {
    result <- calcSteepnessOverlap(mod = NULL, obj = NULL)
    expect_equal(result, c(NA, NA))
  })

  it("returns correct steepness for shallow, medium, and steep", {
    ## For a Shallow dose response!
    mod1 <- drm(Response~Dose,data=dat_shallow,fct=LL.3())
    ## plot(mod1,type="all")
    result <- calcSteepnessOverlap(mod = mod1, trend = "Decrease")
    expect_equal(result[2], "Shallow")


    ## For a steep dose response!
    mod1 <- drm(Response~Dose,data=dat_steep,fct=LL.3())
    ## plot(mod1,type="all")
    result <- calcSteepnessOverlap(mod = mod1, trend = "Decrease")
    expect_equal(result[2], "Steep")

    ## For a medium dose response!
    mod1 <- drm(Response~Dose,data=dat_medium,fct=LL.3())
    ## plot(mod1,type="all")
    result <- calcSteepnessOverlap(mod = mod1, trend = "Decrease")
    expect_equal(result[2], "Medium")

    # Additional tests can be added as necessary
  })
})


describe("simDRdata function", {

  it("should return a data frame with correct dimensions", {
    result <- simDRdata(10, LL.3(), c(`b:(Intercept)` = 3, `d:(Intercept)` = 8, `e:(Intercept)` = 3),
                        xerror = c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
                                   3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
                        yerror = "rnorm", ypar = c(0, 0.6))

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 10 * 24) # 10 simulations, 24 xerror.
    expect_equal(ncol(result), 3) # Dose, Response, Sim
  })

  it("should return still a data frame when onlyY is TRUE", {
    result <- simDRdata(5, LL.3(), c(`b:(Intercept)` = 3, `d:(Intercept)` = 8, `e:(Intercept)` = 3),
                        xerror = c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
                                   3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
                        yerror = "rnorm", ypar = c(0, 0.6), onlyY = TRUE)

    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3) # Only Response should be returned
  })

  it("should handle different error distributions", {
    result_normal <- simDRdata(5, LL.3(), c(`b:(Intercept)` = 3, `d:(Intercept)` = 8, `e:(Intercept)` = 3),
                               xerror = c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
                                          3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
                               yerror = "rnorm", ypar = c(0, 0.6))

    result_binomial <- simDRdata(5, LL.2(), c(`b:(Intercept)` = -2,  `e:(Intercept)` = 3),
                                 xerror = c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
                                            3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
                                 yerror = "rbinom", ypar = rep(10,24)) # Example for binomial

    expect_s3_class(result_normal, "data.frame")
    expect_s3_class(result_binomial, "data.frame")
  })

})


describe("ECx_rating", {
  it("correctly assigns ratings for various normalized widths", {
    test_values <- c(0.1, 0.3, 0.7, 1.5, 2.5, NA)
    expected_ratings <- c("Excellent", "Good", "Fair", "Poor", "Bad", "Not defined")

    result <- ECx_rating(test_values)
    expect_equal(result, expected_ratings)
  })

  it("handles edge cases correctly", {
    expect_equal(ECx_rating(0.2), "Good")
    expect_equal(ECx_rating(2), "Bad")
    expect_equal(ECx_rating(NA), "Not defined")
    expect_equal(ECx_rating(NaN), "Not defined")
  })
})

describe("drcCompare", {
  it("throws error when neither modRes nor modList is provided", {
    expect_error(
      drcCompare(trend = "Decrease"),
      "Need the model output list from previous step!"
    )
  })

  it("validates CI parameter", {
    expect_error(
      drcCompare(modList = list(), trend = "Decrease", CI = "invalid")##,
      #"'arg' should be one of “delta”, “inv”, “bmd-inv”"
    )
  })

  it("ouput the expected results", {
    data("dat_medium")
    dat_medium <- dat_medium %>% mutate(Treatment=factor(Dose,levels=unique(Dose)))
    dat_medium$Response[dat_medium$Response < 0] <- 0

    mod <- drm(Response~Dose,data=dat_medium,fct=LL.3())

    fctList <- list(LN.4(),W1.3(),LL2.2())
    res <- mselect.plus(mod,fctList = fctList )
    set.seed(123)
    res1 <- drcCompare(modRes=res)
    res2 <- drcCompare(modRes=res)
    expect_equal(res1,res2)
    expect_equal(res1$Steepness,c("Medium", "Medium", "Medium", "Steep"))

  })
})


## testthat::test_file("tests/testthat/test_drc_helpers.R")
