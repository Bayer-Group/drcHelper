
# Unit tests for the preliminary assessment functions
library(testthat)

describe("Preliminary Assessment of Dose Response Data", {

  testdata <- data.frame(Dose = c(0, 1, 2, 3, 0, 1, 2, 3),
                         Response = c(10, 8, 6, 4, 10, 9, 7, 5))

  it("prelimPlot1 should return a ggplot object",{
    p <- prelimPlot1(testdata)
    expect_s3_class(p, "gg")

  })

  it("prelimPlot2 should return a ggplot object with log1p transformation",{
    p <- prelimPlot2(testdata,dosecol="Dose")
    expect_s3_class(p, "gg")

  })

  it("prelimSummary should return a data frame with summary statistics", {
    summary <- prelimSummary(testdata)
    expect_true(is.data.frame(summary))
    expect_equal(nrow(summary), 4)  # There are 4 unique doses (0, 1, 2, 3)
    expect_true(all(c("Mean", "SD", "% Inhibition", "CV") %in% colnames(summary)))
  })

})
