library(testthat)

# --- Helper: create reproducible AMA-like test data ---
make_ama_data <- function(seed = 42) {
  set.seed(seed)
  data.frame(
    stage = c(
      # Control: most animals at high stages (57-62)
      sample(54:62, 30, replace = TRUE, prob = c(1, 1, 1, 2, 3, 5, 6, 6, 5)),
      # Low dose: slight shift downward
      sample(54:62, 30, replace = TRUE, prob = c(1, 2, 2, 3, 4, 5, 5, 4, 3)),
      # Mid dose: moderate shift
      sample(54:62, 30, replace = TRUE, prob = c(2, 3, 4, 4, 4, 3, 3, 2, 1)),
      # High dose: strong shift to low stages
      sample(54:62, 30, replace = TRUE, prob = c(4, 5, 5, 4, 3, 2, 1, 1, 1))
    ),
    dose = factor(
      rep(c("Control", "0.1", "1.0", "10"), each = 30),
      levels = c("Control", "0.1", "1.0", "10")
    ),
    tank = rep(rep(c("A", "B", "C"), each = 10), 4),
    stringsAsFactors = FALSE
  )
}


# ===========================================================================
# step_down_jt() tests
# ===========================================================================
describe("step_down_jt()", {
  it("returns correct structure", {
    set.seed(1)
    resp <- c(rnorm(8, 5), rnorm(8, 7), rnorm(8, 9))
    grp <- factor(rep(c("C", "L", "H"), each = 8), levels = c("C", "L", "H"))

    res <- step_down_jt(resp, grp, alternative = "greater")

    expect_type(res, "list")
    expect_named(res, c("summary", "NOEC", "LOEC"))
    expect_s3_class(res$summary, "data.frame")
    expect_true(all(
      c(
        "groups_included",
        "highest_dose",
        "JT_statistic",
        "Z_statistic",
        "p_value",
        "significant"
      ) %in%
        names(res$summary)
    ))
    expect_type(res$NOEC, "character")
    expect_type(res$LOEC, "character")
  })

  it("detects a clear increasing trend", {
    set.seed(10)
    resp <- c(rnorm(10, 0), rnorm(10, 5), rnorm(10, 10))
    grp <- factor(rep(c("0", "1", "2"), each = 10), levels = c("0", "1", "2"))

    res <- step_down_jt(resp, grp, alternative = "greater")

    # Full model should be significant
    expect_true(res$summary$significant[1])
  })

  it("returns highest dose as NOEC when no trend", {
    set.seed(99)
    resp <- rnorm(30, mean = 5)
    grp <- factor(rep(c("C", "L", "H"), each = 10), levels = c("C", "L", "H"))

    res <- step_down_jt(resp, grp, alternative = "greater")

    # No trend: NOEC should be the highest dose
    expect_equal(res$NOEC, "H")
    expect_true(is.na(res$LOEC))
  })

  it("step-down proceeds in correct order (high to low)", {
    set.seed(1)
    resp <- c(rnorm(6, 0), rnorm(6, 3), rnorm(6, 6), rnorm(6, 9))
    grp <- factor(
      rep(c("C", "L", "M", "H"), each = 6),
      levels = c("C", "L", "M", "H")
    )

    res <- step_down_jt(resp, grp, alternative = "greater")

    # First row should test all 4 groups, last row should test C vs L
    expect_true(grepl("H", res$summary$highest_dose[1]))
    expect_true(grepl("L", res$summary$highest_dose[nrow(res$summary)]))
  })

  it("errors with fewer than 2 groups", {
    resp <- rnorm(5)
    grp <- factor(rep("C", 5))

    expect_error(step_down_jt(resp, grp), "at least 2 groups")
  })
})


# ===========================================================================
# mqjt_test() tests
# ===========================================================================
describe("mqjt_test()", {
  ama <- make_ama_data()

  it("returns correct class and structure", {
    res <- mqjt_test(ama, "stage", "dose", "tank", alternative = "less")

    expect_s3_class(res, "mqjtTest")
    expect_named(
      res,
      c(
        "slice_results",
        "overall_NOEC",
        "overall_LOEC",
        "noec_per_slice",
        "cutpoints",
        "detail",
        "alternative",
        "alpha"
      )
    )
    expect_s3_class(res$slice_results, "data.frame")
    expect_s3_class(res$noec_per_slice, "data.frame")
    expect_type(res$overall_NOEC, "character")
    expect_type(res$overall_LOEC, "character")
    expect_type(res$detail, "list")
  })

  it("auto-generates cutpoints from control when not specified", {
    res <- mqjt_test(ama, "stage", "dose", "tank", alternative = "less")

    # Cutpoints should be derived from unique control stages (minus the min)
    ctrl_stages <- sort(unique(ama$stage[ama$dose == "Control"]))
    expected_cuts <- ctrl_stages[ctrl_stages > min(ctrl_stages)]

    expect_equal(res$cutpoints, expected_cuts)
  })

  it("respects user-supplied cutpoints", {
    custom_cuts <- c(56, 58, 60)
    res <- mqjt_test(
      ama,
      "stage",
      "dose",
      "tank",
      cutpoints = custom_cuts,
      alternative = "less"
    )

    expect_equal(res$cutpoints, custom_cuts)
    expect_equal(nrow(res$noec_per_slice), length(custom_cuts))
  })

  it("noec_per_slice has one row per cutpoint", {
    res <- mqjt_test(ama, "stage", "dose", "tank", alternative = "less")

    expect_equal(nrow(res$noec_per_slice), length(res$cutpoints))
    expect_equal(res$noec_per_slice$cutpoint, res$cutpoints)
  })

  it("overall NOEC is the most conservative (lowest) across slices", {
    res <- mqjt_test(ama, "stage", "dose", "tank", alternative = "less")

    # The overall NOEC should match the minimum NOEC position in dose levels
    dose_levels <- levels(ama$dose)
    noec_positions <- match(res$noec_per_slice$NOEC, dose_levels)
    noec_positions[is.na(noec_positions)] <- 0L
    min_noec <- res$noec_per_slice$NOEC[which.min(noec_positions)]

    expect_equal(res$overall_NOEC, min_noec)
  })

  it("handles character dose column by coercing to factor", {
    ama_chr <- ama
    ama_chr$dose <- as.character(ama_chr$dose)

    expect_no_error(
      mqjt_test(ama_chr, "stage", "dose", "tank", alternative = "less")
    )
  })

  it("handles numeric dose column", {
    ama_num <- ama
    ama_num$dose <- c(rep(0, 30), rep(0.1, 30), rep(1, 30), rep(10, 30))

    res <- mqjt_test(ama_num, "stage", "dose", "tank", alternative = "less")

    expect_s3_class(res, "mqjtTest")
    # Factor levels should be in numeric order
    expect_equal(levels(factor(ama_num$dose))[1], "0")
  })

  it("errors when columns are missing", {
    expect_error(
      mqjt_test(ama, "nonexistent", "dose", "tank"),
      "not found"
    )
    expect_error(
      mqjt_test(ama, "stage", "nonexistent", "tank"),
      "not found"
    )
  })

  it("errors with non-data-frame input", {
    expect_error(
      mqjt_test(list(a = 1), "a", "b", "c"),
      "data frame"
    )
  })

  it("errors with only one dose level", {
    one_dose <- ama[ama$dose == "Control", ]
    one_dose$dose <- factor(one_dose$dose)

    expect_error(
      mqjt_test(one_dose, "stage", "dose", "tank"),
      "at least 2"
    )
  })

  it("use_replicate_means = FALSE runs on individual-level data", {
    res <- mqjt_test(
      ama,
      "stage",
      "dose",
      "tank",
      cutpoints = c(58, 60),
      alternative = "less",
      use_replicate_means = FALSE
    )

    expect_s3_class(res, "mqjtTest")
    expect_equal(length(res$detail), 2)
  })

  it("print method runs without error", {
    res <- mqjt_test(
      ama,
      "stage",
      "dose",
      "tank",
      cutpoints = c(58, 60),
      alternative = "less"
    )
    expect_output(print(res), "MQJT")
  })

  it("summary method runs without error", {
    res <- mqjt_test(
      ama,
      "stage",
      "dose",
      "tank",
      cutpoints = c(58, 60),
      alternative = "less"
    )
    expect_output(summary(res), "MQJT")
  })
})


# ===========================================================================
# Edge cases
# ===========================================================================
describe("MQJT edge cases", {
  it("handles all animals at the same stage in control", {
    dat <- data.frame(
      stage = c(rep(60, 20), rep(58, 10), rep(56, 10)),
      dose = factor(rep(c("C", "L"), c(20, 20)), levels = c("C", "L")),
      tank = rep(rep(c("A", "B"), each = 10), 2)
    )

    # All control animals at stage 60 -> only cutpoint would be 60 itself
    expect_warning(
      res <- mqjt_test(dat, "stage", "dose", "tank", alternative = "less"),
      "same stage"
    )
    expect_s3_class(res, "mqjtTest")
  })

  it("handles NAs in stage column gracefully", {
    ama <- make_ama_data()
    ama$stage[c(5, 15, 25)] <- NA

    res <- mqjt_test(
      ama,
      "stage",
      "dose",
      "tank",
      cutpoints = c(58, 60),
      alternative = "less"
    )

    expect_s3_class(res, "mqjtTest")
  })
})
