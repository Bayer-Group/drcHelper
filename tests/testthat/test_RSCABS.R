library(testthat)
describe("RSCABS",{
  # Mock data for testing
  set.seed(123)
  mock_data <- data.frame(
    Treatment = factor(rep(c("Control", "Dose1", "Dose2"), each = 10)),
    Replicate = rep(1:10, 3),
    Endpoint1 = c(rpois(10, 2), rpois(10, 3), rpois(10,5)),
    Endpoint2 = c(rpois(10, 1), rpois(10, 2), rpois(10, 3))
  )
  #mock_data <- pvi_example %>% mutate(y0 = as.numeric(factor(y0)))
  # Unit tests
  it("runRSCABS calculates correctly", {
    result <- runRSCABS(as.data.frame(mock_data), "Treatment", "Replicate",Effects = "Endpoint1")

    expect_s3_class(result, "data.frame")
    expect_true("Statistic" %in% colnames(result))
    expect_true("P.Value" %in% colnames(result))
    expect_identical(result$Signif,c(".", ".", "**", ".", "**", ".", "***", "*", "**", ".", "*",
                                     "*", ".", ".", "."))
  })

  it("RSCABK calculates correctly for RS type", {
    x.i.j <- matrix(c(5, 3, 4, 2), nrow = 2)
    n.i.j <- matrix(c(10, 10, 10, 10), nrow = 2)
    m.i <- c(2, 2)
    TestK <- 1
    test.type <- "RS"

    result <- RSCABK(x.i.j, n.i.j, m.i, TestK, test.type)

    expect_true("Treatment" %in% names(result))
    expect_true("Statistic" %in% names(result))
    expect_true("P-Value" %in% names(result))
  })

  it("prepDataRSCABS prepares data correctly", {
    mock_data$Replicate <- factor(mock_data$Replicate) ## need Replicate to be changed to factor!!!
    result <- prepDataRSCABS(Effect = "Endpoint1", Data = mock_data, "Treatment", "Replicate")

    expect_true("x.i.j" %in% names(result))
    expect_true("n.i.j" %in% names(result))
    expect_true("m.i" %in% names(result))
  })

  it("convert2Score handles negative values", {
    result <- convert2Score(c(5, -1, 3, 0, NA))

    expect_equal(result, c(5, NA, 3, 0, NA))
  })

})
