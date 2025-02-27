
library(testthat)

describe("contEndpoint Function", {


  it("should return a numeric NOEC value for valid inputs", {
    result <- contEndpoint(paov = 0.01, pks = 0.03, pnormal = 0.06,
                           phomogeneity = 0.07, monotonicity = c(0.1,0.2),
                           william = c(0.07, 0.06, 0.03, 0.01), dunnett = c(0.07, 0.06, 0.03, 0.01),
                           dunn = c(0.07, 0.06, 0.03, 0.01),
                           jonckheere = c(0.07, 0.06, 0.03, 0.01),
                           procedure = "stepDown",
                           doses = c("Control","A", "B", "C", "D"))
    expect_true(is.numeric(result))
  })

  it("should return an attribute indicating the test used", {
    result <- contEndpoint(paov = 0.01, pks = 0.03, pnormal = 0.06,
                           phomogeneity = 0.07, monotonicity = c(0.1,0.2),
                           william = c(0.07, 0.06, 0.03, 0.01), dunnett = c(0.07, 0.06, 0.03, 0.01),
                           dunn = c(0.07, 0.06, 0.03, 0.01),
                           jonckheere = c(0.07, 0.06, 0.03, 0.01),
                           procedure = "stepDown",
                           doses = c("Control","A", "B", "C", "D"))
    expect_true("test" %in% names(attributes(result)))
  })

})
