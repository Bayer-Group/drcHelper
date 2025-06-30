# tests/testthat/test-broom_functions.R

library(testthat)
library(tibble)
library(dplyr)

# Create test data that will be used across multiple tests
set.seed(123)
create_test_data <- function() {
  data.frame(
    dose = factor(rep(c(0, 1, 5, 10), each = 5)),
    response = c(rnorm(5, 100, 10), rnorm(5, 90, 10),
                 rnorm(5, 80, 10), rnorm(5, 70, 10))
  )
}

describe("broom_williams function", {
  it("returns a tibble with the correct structure for PMCMRplus method", {
    skip_if_not_installed("PMCMRplus")
    set.seed(123)
    test_data <- create_test_data()
    result <- broom_williams(response ~ dose, data = test_data, method = "Williams_PMCMRplus")
    # Mock the PMCMRplus::williamsTest function to avoid actual computation
    ## with_mocked_bindings(

    res0 <- structure(list(comparison = c("1 - 0  <= 0", "5 - 0  <= 0", "10 - 0  <= 0"
    ), estimate = c(`1 - 0` = -12.3788923368534, `5 - 0` = -18.8566852968345,
                    `10 - 0` = -30.8422807049215), `t'-stat` = c(`1 - 0` = -1.86377216687559,
                                                                 `5 - 0` = -2.83907188619315,
                                                                 `10 - 0` = -4.64362907250297),
    `t'-crit` = c(1.746, 1.831, 1.86), decision = c("accept", "accept", "accept"),
    method = c("Williams_PMCMRplus", "Williams_PMCMRplus", "Williams_PMCMRplus")),
    class = c("tbl_df","tbl", "data.frame"), row.names = c(NA, -3L))
    expect_equal(res0,result,tolerance = 1e-4)
    expect_s3_class(result, "tbl_df")
    expect_named(result, c("comparison", "estimate", "t'-stat", "t'-crit", "decision", "method"))
    expect_equal(result$method, rep("Williams_PMCMRplus", nrow(result)))

    ##)
  })

  it("returns a tibble with the correct structure for JG method", {
    skip_if_not_installed("drcHelper")
    set.seed(123)
    test_data <- create_test_data()

    result <- broom_williams(response ~ dose, data = test_data, method = "Williams_JG")

    expect_s3_class(result, "tbl_df")
    expect_true("comparison" %in% names(result))
    expect_true("method" %in% names(result))
    expect_equal(result$method, rep("Williams_StatCharrms", nrow(result)))
    expect_equal(result$estimate,c(-12.3757, -18.8557, -30.8457),tolerance = 1e-4)
    expect_equal(result$`t'-stat`,c(1.864, 2.839, 4.644),tolerance = 1e-4)
  }
  )

})

describe("broom_dunnett function", {
  test_data <- create_test_data()
  result_1 <- broom_dunnett(response ~ dose, data = test_data, method = "Dunnett_DescTools")
  result_2 <- broom_dunnett(response ~ dose, data = test_data, method = "Dunnett_multcomp")
  result_3 <- broom_dunnett(response ~ dose, data = test_data, method = "Dunnett_PMCMRplus")
  it("returns a tibble with the correct structure for multcomp method", {
    skip_if_not_installed("multcomp")
    expect_equal(as.numeric(result_1$estimate),as.numeric(result_2$estimate),tolerance = 1e-4)

    # Mock the multcomp functions

  })

  it("returns a tibble with the correct structure for DescTools method", {
    skip_if_not_installed("DescTools")

    expect_equal(as.numeric(result_1$p.value),as.numeric(result_2$p.value),tolerance = 1e-3)
    expect_equal(as.numeric(result_1$conf.low),as.numeric(result_2$conf.low),tolerance = 1e-3)
    expect_equal(as.numeric(result_1$conf.high),as.numeric(result_2$conf.high),tolerance = 1e-3)




  })

  it("returns a tibble with the correct structure for PMCMRplus method", {
    skip_if_not_installed("PMCMRplus")

    expect_equal(as.numeric(result_3$p.value),as.numeric(result_2$p.value),tolerance = 1e-1)


  })

  it("handles errors gracefully and returns empty tibble", {
    test_data <- create_test_data()

    # Force an error by passing invalid arguments
    res <- suppressWarnings(broom_dunnett(1:10, method = "Dunnett_multcomp"))
    expect_s3_class(res, "tbl_df")
  })

})

# tests/testthat/test-broom_edge_cases.R

describe("broom_williams edge cases", {
  it("handles formula with no data argument", {
    # Create data in the global environment
    test_data <- create_test_data()
    response <- test_data$response
    dose <- test_data$dose

    # Test without explicitly providing data
    expect_no_error(
      result <- broom_williams(response ~ dose)
    )
  })

})

describe("broom_dunnett edge cases", {
  it("handles aov object input", {
    test_data <- create_test_data()
    aov_model <- aov(response ~ dose, data = test_data)

    expect_no_error(
      result <- broom_dunnett(aov_model, method = "Dunnett_multcomp")
    )
    expect_s3_class(result, "tbl_df")
  }
  )


  it("handles lm object input", {
    test_data <- create_test_data()
    lm_model <- lm(response ~ dose, data = test_data)

    expect_no_error(
      result <- broom_dunnett(lm_model, method = "Dunnett_multcomp")
    )
    expect_s3_class(result, "tbl_df")
  }
  )




  it("handles control argument with error", {
    test_data <- create_test_data()

    expect_warning(
      broom_dunnett(response ~ dose, data = test_data, method = "Dunnett_multcomp", control = 2),
      "Please make sure control is the first level"
    )
  })
})
