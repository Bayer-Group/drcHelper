# Progress — drcHelper Package

> **Purpose**: Track current state of work, recent changes, and next steps. Update at the end of each significant work session.

## Current State

- **Version**: 0.0.5
- **Branch**: `dev`
- **Default branch**: `main`
- **Last updated**: 2026-03-06 (session 2)

## Recent Changes (v0.0.5)

- Migrated all `%>%` → `|>` in 12 R source files and 2 test files
- Removed `magrittr` from DESCRIPTION Imports
- Stripped `R/utils-pipe.R` of magrittr re-export (now only `globalVariables` hack)
- Fixed 4 mangled `@importFrom` roxygen tags (removed invalid `|>` tokens)
- Deleted stale `man/pipe.Rd`
- NAMESPACE regenerated: no more `export("%>%")`, `importFrom(magrittr,...)`, or `importFrom(dplyr,"%>%")`
- Added `tsk_auto` for trimmed Spearman-Kärber LD50 calculation
- Added `SpearmanKarber_modified` for Spearman-Kärber LD50 calculation
- Added `compare_to_control_welch` for Welch's t-test with multiple comparison adjustment
- Added `noec_from_trend_test` wrapper for step-down trend test with NOEC outcome
- Added `compute_mdd_williams` and `compute_mdd_dunnett` for MDD calculations
- Added `report_dunnett_summary` for reporting test results
- Added many vignette articles for concept explanation and verification
- Updated `dunnett_test` to correctly handle non-RCBD replicate IDs
- Added test cases validation reports in `inst/SystemTesting/`
- Set up AI agent support (`.github/copilot-instructions.md`, memory system, prompt templates)

## Known Issues / Technical Debt

- RSCABS duplication: legacy `RSCABS.R` (archived) vs modern `RSCABS_AO.R`
- `REDUNDANCY_ANALYSIS.md` says MQJT.R was removed but it has 439 lines of new code
- Pre-existing test failure: `test_broom.R:150` expects warning not thrown
- `ED.ZG.Rd` and `mselect.ZG.Rd` missing `@title` (roxygen2 warning)
- `R/utils-pipe.R` still contains `globalVariables(".")` hack — can be removed once all `.` usage is confirmed eliminated

## Active Refactoring

See `dev/REFACTORING_PLAN.md` for the full plan. Execution order:

1. [x] Bump R version to `>= 4.1.0`
2. [x] Rename files with typos (`williams_JT.R`, `preliminary.R`, `data_helper.R`, `reshape_drc_data.R`)
3. [x] Add `snake_case` aliases for camelCase exports (29 aliases in `R/zzz_aliases.R`)
4. [x] Migrate `%>%` → `|>` in package code (12 R files, 2 test files)
5. [x] Remove `magrittr` dependency (removed from DESCRIPTION, NAMESPACE, `@importFrom` tags)
6. [ ] Add `.Deprecated()` warnings to camelCase names
7. [ ] Update `REDUNDANCY_ANALYSIS.md` to reflect current state
8. [ ] Update vignettes and README (check for remaining `%>%`)

## Next Steps

- [ ] Execute remaining refactoring plan steps 6–8 (deprecation warnings, REDUNDANCY_ANALYSIS.md, vignettes/README)
- [ ] Check vignettes for remaining `%>%` usage
- [ ] Fix pre-existing test failure `test_broom.R:150`
- [ ] Fix `ED.ZG.Rd` and `mselect.ZG.Rd` missing `@title`
- [ ] Consider removing `globalVariables(".")` from `utils-pipe.R`
- [ ] Continue improving test coverage
- [ ] Prepare for CRAN submission
- [ ] Expand pkgdown articles

---

*Update this file at the end of each work session with significant changes.*
