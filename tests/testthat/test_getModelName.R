describe("getModelName function", {
  it("returns all model names and descriptions when fname is NULL", {
    result <- getModelName()
    expect_true(is.character(result))
    expect_true(length(result) > 10)
    expect_true(any(grepl("LL.2: Log-logistic", result)))
  })

  it("returns correct description for a single model name", {
    result <- getModelName("LL.2")
    expect_true(is.character(result))
    expect_true(grepl("LL.2", result))
  })

  it("returns correct descriptions for multiple model names", {
    result <- getModelName(c("LL.2", "LL.3", "LN.2", "LN.4"))
    expect_true(is.character(result))
    expect_equal(length(result), 4)
    expect_true(any(grepl("LL.2", result)))
    expect_true(any(grepl("LN.2: 2-parameter log-normal", result)))
    expect_true(any(grepl("LN.4: 4-parameter log-normal", result)))
  })
})
