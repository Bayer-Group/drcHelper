# Coding Conventions — drcHelper Package

> **Purpose**: Project-specific coding patterns beyond the general style guide. Read alongside `ARCHITECTURE.md`.

## Function Patterns

### Input validation
Always validate inputs at the top of exported functions:

```r
#' @export
my_function <- function(data, dose_col, response_col) {
  if (!is.data.frame(data)) stop("'data' must be a data.frame")
  if (!dose_col %in% names(data)) stop(paste0("Column '", dose_col, "' not found in data"))
  # ... function body
}
```

### Tidy evaluation
Use `.data` pronoun when referencing column names inside `dplyr` verbs:

```r
data |>
  dplyr::group_by(.data[[dose_col]]) |>
  dplyr::summarise(mean_resp = mean(.data[[response_col]], na.rm = TRUE))
```

### Return values
- Statistical test functions return a named `list` or `data.frame` / `tibble`
- Model-fitting wrappers return the `drc` model object (class `drc`)
- Broom-style tidiers return `tibble`s

### Family tags
Group related functions with `@family`:
- `@family NOEC` — NOEC calculation functions
- `@family DRC` — dose-response curve helpers
- `@family trend_tests` — trend test implementations
- `@family data_helpers` — data manipulation utilities
- `@family quantal` — quantal/binomial endpoint functions

## Testing Patterns

### Standard test file structure
```r
describe("function_name()", {
  it("returns expected result for typical input", {
    result <- function_name(typical_input)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), expected_n)
  })

  it("handles edge case X", {
    expect_warning(function_name(edge_input), "expected warning")
  })

  it("errors on invalid input", {
    expect_error(function_name(bad_input), "informative message")
  })
})
```

### Using package test data
```r
data(test_cases_data, package = "drcHelper")
# or after devtools::load_all():
# test_cases_data is available directly
```

## Naming Conventions

| Pattern | Example | When to use |
|---|---|---|
| `calculate_*` | `calculate_noec` | Computing a numeric result |
| `fit_*` | `fit_model` | Fitting a statistical model |
| `get_*` | `getEC50`, `getEndpoint` | Extracting from an existing object |
| `compare_*` | `compare_to_control_welch` | Statistical comparisons |
| `compute_*` | `compute_mdd_dunnett` | Heavier computations |
| `create_*` | `create_summary_table` | Building new data structures |
| `convert_*` | `convert_fish_data` | Format conversions |
| `expand_*` / `aggregate_*` | `expand_to_individual_tidy` | Reshaping data |

*Note*: Some legacy functions use `camelCase` (e.g., `getEC50`, `calcNW`). New functions should use `snake_case`.

## Documentation Conventions

- All `@examples` should be wrapped in `\donttest{}` if they take > 5 seconds
- Use `@seealso` to cross-reference related functions
- Include `@references` for published methods with DOI when available
- Datasets documented in `R/data_description.R` (single file for all datasets)

## Git Workflow

- `main` — stable release branch
- `dev` — active development branch
- Feature branches from `dev` for significant changes
- Commit messages: imperative mood, reference issue numbers when applicable
