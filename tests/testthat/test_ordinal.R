## ordinal.R

library(testthat)
describe("Back Calculate Standard Error from Lognormal Parameters",{
  it("backCalcSE calculates correctly with approximation", {
    expect_equal(backCalcSE(se = 0.1, mu = 0, approximate = TRUE), exp(0) * 0.1, tolerance = 1e-6)
    expect_equal(backCalcSE(se = 0.5, mu = 1, approximate = TRUE), exp(1) * 0.5, tolerance = 1e-6)
  })

  it("backCalcSE calculates correctly without approximation", {
    expect_equal(backCalcSE(se = 0.1, mu = 0, approximate = FALSE),
                 sqrt((exp(0.1^2) - 1) * exp(2 * 0 + 0.1^2)), tolerance = 1e-6)
    expect_equal(backCalcSE(se = 0.5, mu = 1, approximate = FALSE),
                 sqrt((exp(0.5^2) - 1) * exp(2 * 1 + 0.5^2)), tolerance = 1e-6)
  })

  it("backCalcSE handles invalid inputs", {
    expect_error(backCalcSE(se = "a", mu = 0), "Both 'se' and 'mu' must be numeric values.")
    expect_error(backCalcSE(se = 0.1, mu = "b"), "Both 'se' and 'mu' must be numeric values.")
  })
})

describe("EC50 can be calculated correctly",{
  library(MASS)  # For the dose.p function

  # Mock data for testing
  set.seed(123)
  mock_data <- data.frame(
    dose = rep(c(0, 0.1, 1, 10), each = 10),
    response = c(rbinom(10, 1, 0.1), rbinom(10, 1, 0.3), rbinom(10, 1, 0.7), rbinom(10, 1, 0.9))
  )

  # Fit a glm model for testing
  mock_glm <- glm(response ~ log(dose + 0.01), family = binomial, data = mock_data)

  # Unit tests
  it("getEC50 calculates correctly for glm", {
    ec50_result <- getEC50(mock_glm)

    expect_s3_class(ec50_result, "data.frame")
    expect_named(ec50_result, c("EC50", "lower", "upper", "se"))
    expect_true(all(ec50_result[c("EC50", "lower", "upper", "se")] >= 0))
    expect_identical(ec50_result,structure(list(EC50 = 0.427817535691421, lower = 0.108614708265297,
                                                upper = 1.68511103853472, se = 0.43402147887266), class = "data.frame", row.names = c(NA,
                                                                                                                                      -1L)),
                     tolerance = 1e-4)
  })

  it("getEC50 stops for invalid model types", {
    expect_error(getEC50(lm(mpg ~ wt, data = mtcars)), "The model must be of class 'glm' or 'glmmPQL'.")
  })

  # If you have a glmmPQL model, you can add tests for that as well
  # it("getEC50 calculates correctly for glmmPQL", {
  #   # Fit a glmmPQL model here and test similarly
  # })
  glmm_model <- glmmPQL(yt ~ log(dose),random= ~1 | Obs,family= quasibinomial(link="logit"),data=pvi_example)
  it("dose.p.glmmPQL calculates correctly for glmmPQL", {
    # Fit a glmmPQL model here and test similarly
    dose_result <- dose.p.glmmPQL(glmm_model)
    expect_identical(dose_result,structure(c(`p = 0.5:` = 2.52401512277109),
                                           SE = structure(0.0671336307260389,
                                                          dim = c(1L, 1L), dimnames = list("p = 0.5:", NULL)),
                                           p = 0.5, class = "glm.dose"),
                     tolerance=1e-4)
  })
  it("getEC50 calculates correctly for glmmPQL",{
    ec50_result <- getEC50(glmm_model)
    expect_identical(ec50_result,structure(list(EC50 = 12.4785993212837, lower = 10.9400810608234,
                                                upper = 14.2334814664911, se = 0.840570527519782), class = "data.frame", row.names = "p = 0.5:"),
                     tolerance=1e-4)
  })
})

