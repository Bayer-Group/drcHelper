---
applyTo: '**/*.R'
---

# R Code Guidelines — drcHelper

## AI Agent Bootstrap

**At the start of every new chat session**, read these memory files:
1. `.github/memory/ARCHITECTURE.md` — Module map
2. `.github/memory/CONVENTIONS.md` — Coding patterns
3. `.github/memory/PROGRESS.md` — Current state

## R Package Development Commands

Use these devtools commands — never source files manually:

- `devtools::load_all()` — Load package for interactive testing
- `devtools::document()` — Regenerate NAMESPACE and man/ pages
- `devtools::test()` — Run all tests
- `devtools::test(filter = "name")` — Run specific test file
- `devtools::check()` — Full R CMD check

## Code Style

- `snake_case` for new functions (legacy `camelCase` exists but don't extend it)
- `%>%` pipe (magrittr), not `|>`
- `.data` pronoun in dplyr for tidy eval
- `@export` in roxygen for public functions
- `@noRd` for internal helpers
- `describe()`/`it()` blocks in testthat

## Roxygen2 Template

```r
#' Title in sentence case
#'
#' Longer description of what the function does.
#'
#' @param x Description of x.
#' @param y Description of y.
#'
#' @return Description of return value.
#'
#' @export
#' @family family_tag
#'
#' @examples
#' \donttest{
#' result <- my_function(x = 1, y = 2)
#' }
my_function <- function(x, y) {
  if (!is.numeric(x)) stop("'x' must be numeric")
  # implementation
}
```

## Test Template

```r
describe("my_function()", {
  it("returns correct result for typical input", {
    result <- my_function(x = 1, y = 2)
    expect_equal(result, expected_value)
  })

  it("errors on invalid input", {
    expect_error(my_function(x = "bad"), "'x' must be numeric")
  })
})
```

## Common Patterns

### Data validation at function entry
```r
if (!is.data.frame(data)) stop("'data' must be a data.frame")
if (!dose_col %in% names(data)) stop(sprintf("Column '%s' not found", dose_col))
```

### Tidy evaluation with column names as strings
```r
data %>%
  dplyr::group_by(.data[[group_col]]) %>%
  dplyr::summarise(mean_val = mean(.data[[value_col]], na.rm = TRUE))
```

### Returning structured results
```r
structure(
  list(
    estimate = est,
    conf.low = ci_low,
    conf.high = ci_high,
    method = "My Method"
  ),
  class = c("my_result", "list")
)
```
