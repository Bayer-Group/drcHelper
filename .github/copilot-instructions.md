# GitHub Copilot Custom Instructions — drcHelper R Package

## Project Identity

- **Package**: `drcHelper` — helper functions for dose-response analysis in toxicology/ecotoxicology
- **Language**: R (tidyverse style)
- **Build system**: `devtools` / `roxygen2` / `testthat` (edition 3)
- **Key dependencies**: `drc`, `drcData`, `bmd`, `multcomp`, `PMCMRplus`, `ggplot2`, `dplyr`
- **License**: GPL (>= 3)
- **CI**: GitHub Actions (`R-CMD-check`, `pkgdown`)

## R Package Development Workflow

When helping with this project, always use the standard R package development workflow:

| Task | Command |
|---|---|
| Load all functions | `devtools::load_all()` |
| Run tests | `devtools::test()` |
| Run a single test file | `devtools::test(filter = "filename")` |
| Check package | `devtools::check()` |
| Document | `devtools::document()` |
| Build vignettes | `devtools::build_vignettes()` |
| Install locally | `devtools::install()` |
| Build pkgdown site | `pkgdown::build_site()` |
| Add dependency | `usethis::use_package("pkg")` |
| Create test file | `usethis::use_test("function_name")` |
| Create R file | `usethis::use_r("function_name")` |
| Create vignette | `usethis::use_vignette("topic")` |

## Code Conventions

1. **Function style**: `snake_case`, verbs for actions (`calculate_noec`, `fit_model`)
2. **Documentation**: roxygen2 with `@param`, `@return`, `@export`, `@examples`, `@family` tags
3. **Testing**: `testthat` edition 3 with `describe()`/`it()` blocks
4. **Pipe**: magrittr `%>%` (not native `|>`) — the package imports `magrittr`
5. **Data manipulation**: `dplyr` verbs, `.data` pronoun for tidy evaluation
6. **Plotting**: `ggplot2` with `theme_bw()` as default theme
7. **Error handling**: Use `stop()`, `warning()`, `message()` with informative text; validate inputs early
8. **Internal functions**: Prefix with `.` or don't `@export`; document with `@noRd` if truly internal

## File Organization

| Path | Purpose |
|---|---|
| `R/` | Package source code |
| `tests/testthat/` | Unit tests |
| `man/` | Auto-generated docs (never edit by hand) |
| `vignettes/` | Long-form documentation |
| `data/` | Exported `.rda` datasets |
| `data-raw/` | Scripts to create datasets |
| `inst/` | Installed files (system tests, archives, notes) |
| `dev/` | Development scratch scripts (not in package) |
| `.github/instructions/` | Copilot instruction files |
| `.github/memory/` | AI agent memory files (see below) |
| `.github/prompts/` | Reusable prompt templates |

## AI Memory System

This project uses `.github/memory/` to persist context across Copilot chat sessions:

- **`ARCHITECTURE.md`** — Package structure, module responsibilities, key design patterns
- **`DECISIONS.md`** — Log of significant design/implementation decisions with rationale
- **`PROGRESS.md`** — Current state of work, recent changes, next steps
- **`CONVENTIONS.md`** — Coding patterns specific to this project (beyond style guide)

**When starting a new chat session**, read the memory files first to get full context.
**Before ending a session** with significant work, update `PROGRESS.md` and `DECISIONS.md` if new decisions were made.

## Common Patterns in This Package

### Adding a new exported function
1. Create `R/function_name.R` (or add to relevant existing file)
2. Write roxygen2 docs with `@export`
3. Run `devtools::document()`
4. Create `tests/testthat/test-function_name.R`
5. Write tests, run `devtools::test(filter = "function_name")`
6. Update `NEWS.md`

### Adding a new dataset
1. Write creation script in `data-raw/DATASET.R`
2. Save with `usethis::use_data(dataset_name, overwrite = TRUE)`
3. Document in `R/data_description.R`
4. Run `devtools::document()`

### Modifying existing functions
1. Read current code + tests + docs
2. Make changes
3. Run `devtools::test()` to verify nothing breaks
4. Update docs if signature changed
5. Run `devtools::check()` before committing
