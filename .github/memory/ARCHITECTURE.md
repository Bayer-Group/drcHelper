# Architecture — drcHelper Package

> **Purpose**: Persistent record of package structure and design. Read this at the start of each AI chat session.

## Package Overview

`drcHelper` provides helper functions for dose-response curve (DRC) analysis in toxicology and ecotoxicology. It wraps and extends the `drc` package with utilities for:

- **Dose-response model fitting** (`drc_Helper.R`, `Endpoints.R`)
- **NOEC determination** via multiple statistical approaches (`NOEC.R`, `continuous_tests.R`, `quantal_tests.R`)
- **Trend tests** — Jonckheere-Terpstra, Williams, step-down approaches (`williams_JT.R`, `TrendTest_JG.R`, `stepDownTrendTest_wrapper.R`, `stepdown_binom.R`)
- **Pairwise comparisons** — Dunnett, Dunn, Fisher (`dunnett.R`, `dunn_test.R`)
- **Overdispersion handling** for binomial data (`overdispersion_binom.R`, `integrated_tarone.R`)
- **Data reshaping** between individual and summary formats (`reshape_drc_data.R`, `data_helper.R`)
- **Spearman-Kärber / TSK** methods (`brsr_tsk.R`, `SK_TSK_tests_wrapper.R`)
- **MDD calculations** (`MDD.R`)
- **Broom-style tidiers** for test results (`broom.R`)
- **Simulation** of dose-response data (`dose_response_simulation.R`)
- **Statistical characteristics** (`StatCharrms.R`)
- **RSCABS** — Rao-Scott adjusted Cochran-Armitage (`RSCABS.R`, `RSCABS_AO.R`)
- **Ordinal data analysis** (`ordinal.R`)
- **C-alpha test** for overdispersion (`integrated_calpha.R`)

## Key Source Files

| File | Responsibility |
|---|---|
| `R/drc_Helper.R` | Core DRC fitting helpers, model comparison |
| `R/Endpoints.R` | ECx/EDx estimation with confidence intervals |
| `R/NOEC.R` | NOEC wrapper logic |
| `R/continuous_tests.R` | Tests for continuous endpoints |
| `R/quantal_tests.R` | Tests for quantal (binary) endpoints |
| `R/dunnett.R` | Dunnett's test implementation |
| `R/williams_JT.R` | Williams and Jonckheere-Terpstra tests |
| `R/MDD.R` | Minimum Detectable Difference calculations |
| `R/broom.R` | Tidy output formatters |
| `R/reshape_drc_data.R` | Data format conversion utilities |
| `R/data_helper.R` | Data manipulation helpers |
| `R/data_description.R` | Roxygen docs for exported datasets |
| `R/overdispersion_binom.R` | Binomial overdispersion tools |
| `R/integrated_tarone.R` | Tarone's test for overdispersion |
| `R/integrated_calpha.R` | C-alpha test integration |
| `R/dose_response_simulation.R` | Data simulation for testing |
| `R/preliminary.R` | Preliminary data checks |
| `R/utils-pipe.R` | `globalVariables(".")` declaration (legacy; magrittr re-export removed) |
| `R/zzz_aliases.R` | snake_case aliases for legacy camelCase exports (29 aliases) |
| `R/zzz.R` | Package load hooks |

## Dependencies Architecture

- **Hard deps** (`Depends`): `drc`, `drcData`, `R >= 4.1.0`
- **Imports**: `stats`, `multcomp`, `PMCMRplus`, `dplyr`, `MASS`, `stringr`, `tidyselect`, `tibble`, `lme4`, `nlme`, `bmd`, `ggplot2`, `scales`, `metafor`, `purrr`, `rlang`, `rstatix`, `DescTools`, `isotone`
- **Suggests**: `knitr`, `rmarkdown`, `Iso`, `plyr`, `tidyr`, `MCPMod`, `testthat`
- **Remotes**: Custom forks of `bmd`, `drc`, `drcData` from DoseResponse GitHub org
- **Removed**: `magrittr` (migrated to native `|>` pipe)

## Testing Strategy

- `testthat` edition 3
- Tests in `tests/testthat/`
- System-level validation reports in `inst/SystemTesting/`
- Test data stored in `data/` (e.g., `test_cases_data.rda`, `test_cases_res.rda`)

## Documentation

- Function docs: roxygen2 → `man/` (auto-generated)
- Vignettes: `vignettes/` (Rmd and Qmd)
- pkgdown site: configured via `_pkgdown.yml`
- README: `README.Rmd` → `README.md`

## Current Version

**0.0.5** — see `NEWS.md` for changelog.
