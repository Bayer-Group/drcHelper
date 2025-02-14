library(testthat)
# test_logxp.R
describe("Testing logxp and invlogxp functions",{
  it("logxp function scales the x-axis correctly", {
    x <- c(1, 2, 3)
    a <- 1
    result <- logxp(x, a)
    expect_equal(result, log(a + x))
  })

  it("invlogxp function inversely transforms the scaled x-axis", {
    x <- c(1, 2, 3)
    a <- 1
    result <- invlogxp(log(x + a), a)
    expect_equal(result, x)
  })
})

describe("Testing reshaping and formatting data functions",{
  it("simplifyTreatment",{
    x <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
                     3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L,
                     1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L,
                     5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,
                     2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L,
                     6L), levels = c("Control", "1", "3.51", "12.300000000000001",
                                     "42.9", "150"), class = "factor")
    x1 <- simplifyTreatment(x)
    expect_identical(levels(x1),c("Control", "1", "3.51", "12.3", "42.9", "150"))
  })

  it("treatment2dose converts correctly", {
    expect_equal(treatment2dose(c("Control", "0.1", "1", "10")), c(0, 0.1, 1, 10))
    expect_equal(treatment2dose(c("Control", "2", "3.5")), c(0, 2, 3.5))
    expect_equal(treatment2dose(c("Control", "Control", "0", "5")), c(0, 0, 0, 5))
  })

  it("reshape_drcData reshapes correctly", {
    # Create a mock data frame for testing
    mock_data <- data.frame(
      Replicates = c(1, 2, 3),
      Control = c(10, 12, 11),
      `0.1` = c(15, 16, 14),
      `1` = c(20, 22, 19),
      check.names=FALSE
    )
    require(tidyr)
    long_data <- reshape_drcData(mock_data)

    expect_equal(ncol(long_data), 4)  # Check for 4 columns: Replicates, Treatment, Response, Dose
    expect_true("Treatment" %in% colnames(long_data))
    expect_true("Response" %in% colnames(long_data))
    expect_true("Dose" %in% colnames(long_data))

    # Check if the Dose column is correctly calculated
    expected_doses <- c(0, 0.1, 1, 0, 0.1, 1, 0, 0.1, 1)
    expect_equal(long_data$Dose, expected_doses)
  })

  it("reshape_drcData handles NA values", {
    # Create a mock data frame with NA values
    mock_data_with_na <- data.frame(
      Replicates = c(1, 2, 3),
      Control = c(10, NA, 11),
      `0.1` = c(NA, 16, 14),
      `1` = c(20, 22, NA),
      check.names = FALSE
    )

    long_data_with_na <- reshape_drcData(mock_data_with_na)

    expect_true(all(!is.na(long_data_with_na$Response)))  # Ensure there are no NA responses
  })

})



