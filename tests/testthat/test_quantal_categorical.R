## using BDD testing

describe("calcTaronesTest",{
  it("Tarone's Test can be calculated",{
    mymatrix1 <- matrix(c(4,5,5,103),nrow=2,byrow=TRUE)
    colnames(mymatrix1) <- c("Disease","Control")
    rownames(mymatrix1) <- c("Exposure","Unexposed")
    mymatrix2 <- matrix(c(10,3,5,43),nrow=2,byrow=TRUE)
    colnames(mymatrix2) <- c("Disease","Control")
    rownames(mymatrix2) <- c("Exposure","Unexposed")
    mylist <- list(mymatrix1,mymatrix2)
    res <- calcTaronesTest(mylist)
    expect_equal(res$pval,0.6274207,tolerance=1e-4)
    ## calcTaronesTest(mymatrix1)
  })
  it("Tarone's Test can not be run if there is less than 2 stratas",{

    expect_error( calcTaronesTest(mymatrix1))
  })
})



describe("Tarone.test",{
  it("Tarone.test calculates correctly for valid inputs", {
    N <- c(30, 32, 40, 28, 29, 35, 30, 34, 31, 39)
    M <- c(9, 10, 22, 15, 8, 19, 16, 19, 15, 10)

    result <- Tarone.test(N, M)

    expect_s3_class(result, "htest")
    expect_named(result, c("method", "data.name", "null.value", "alternative", "estimate", "statistic", "p.value"))
    expect_equal(result$method, "Tarone's Z test")

    # Check if the statistic is calculated correctly (you may want to calculate expected values)
    expected_statistic <- ((sum((M - N * (sum(M) / sum(N)))^2 / ((sum(M) / sum(N)) * (1 - (sum(M) / sum(N)))))) - sum(N)) / sqrt(2 * sum(N * (N - 1)))
    expect_equal(as.numeric(result$statistic), expected_statistic, tolerance = 1e-6)
  })

  it("Tarone.test stops for invalid inputs", {
    expect_error(Tarone.test(c(30, 32, 40), c(9, 10, -1)), "Count values cannot be negative")
    expect_error(Tarone.test(c(30, 32, 40), c(9, 10, 50)), "Observed count value exceeds number of trials")
    expect_error(Tarone.test(c(30.5, 32, 40), c(9, 10, 22)), "Number of trials should be integers")
    expect_error(Tarone.test(c(30, 32, 40), c(9, 10, 22.5)), "Count values should be integers")
  })
})

