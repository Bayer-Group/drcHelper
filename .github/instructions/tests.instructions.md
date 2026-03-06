---
applyTo: 'tests/**'
---

# Testing Guidelines — drcHelper

## AI Agent Bootstrap

Read `.github/memory/CONVENTIONS.md` for testing patterns before writing tests.

## Test Framework

- `testthat` edition 3
- BDD-style: `describe()` / `it()` blocks
- One test file per source file: `test-{source_file_name}.R`

## Test Structure

```r
describe("function_name()", {
  # Setup shared test data
  test_data <- data.frame(
    dose = c(0, 1, 2, 4, 8),
    response = c(100, 95, 80, 50, 20)
  )

  it("returns expected result for typical input", {
    result <- function_name(test_data)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 5)
  })

  it("handles NA values gracefully", {
    data_with_na <- test_data
    data_with_na$response[3] <- NA
    result <- function_name(data_with_na)
    expect_false(any(is.na(result$estimate)))
  })

  it("errors on invalid input types", {
    expect_error(function_name("not_a_df"), "'data' must be")
  })

  it("warns when sample size is small", {
    small_data <- test_data[1:2, ]
    expect_warning(function_name(small_data), "small sample")
  })
})
```

## Testing Numerical Results

Use `expect_equal()` with `tolerance` for floating-point comparisons:

```r
expect_equal(result$estimate, 3.14, tolerance = 0.01)
```

## Using Package Test Data

```r
data(test_cases_data, package = "drcHelper")
```

## Running Tests

- All tests: `devtools::test()`
- Single file: `devtools::test(filter = "function_name")`
- Never use `source()` or `testthat::test_file()` directly
