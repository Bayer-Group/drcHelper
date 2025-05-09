




library(testthat)
describe("Monotonicy Test",{
  # Mock data for testing
  set.seed(123)
  mock_data <- data.frame(
    treatment_var = factor(rep(c("Control", "Dose1", "Dose2", "Dose3"), each = 10)),
    response_var = c(rnorm(10, mean = 5), rnorm(10, mean = 7), rnorm(10, mean = 8), rnorm(10, mean = 10))
  )
  ## ggplot(mock_data,aes(x=treatment_var,y=response_var))+geom_point()
  # Unit tests
  it("monotonicityTest calculates correctly", {
    result <- monotonicityTest(mock_data, "treatment_var", "response_var")

    expect_s3_class(result, "data.frame")
    expect_named(result, c("Test", "t value", "Pr(>|t|)", "Significance"))
    expect_equal(nrow(result), 2)  # Should have two tests: Linear and Quadratic
  })

  it("getLineContrast returns correct contrasts", {
    result <- getLineContrast(mock_data, "treatment_var")
    expect_equal(result,c(-3, -1, 1, 3))
    expect_equal(length(result), 4)  # Should return contrasts for 4 levels
  })

  it("getQuadContrast returns correct contrasts", {
    result <- getQuadContrast(mock_data, "treatment_var")
    expect_equal(result,c(1, -1, -1, 1))
    expect_equal(length(result), 4)  # Should return contrasts for 3 levels
  })

  it("rankTransform works correctly", {
    transformed_data <- rankTransform(mock_data, "response_var")

    expect_true("TransformedResponse" %in% colnames(transformed_data))  # Check for new column
    expect_equal(nrow(transformed_data), nrow(mock_data))  # Check row count
  })

})
