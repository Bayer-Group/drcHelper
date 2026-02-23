# Extracted from test_broom.R:153

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "drcHelper", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(tibble)
library(dplyr)
set.seed(123)
create_test_data <- function() {
  data.frame(
    dose = factor(rep(c(0, 1, 5, 10), each = 5)),
    response = c(rnorm(5, 100, 10), rnorm(5, 90, 10),
                 rnorm(5, 80, 10), rnorm(5, 70, 10))
  )
}

# test -------------------------------------------------------------------------
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
