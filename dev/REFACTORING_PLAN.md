# drcHelper Refactoring Plan

> Created: 2026-03-06 | Branch: `dev` | Version: 0.0.5

## 1. Function Classification

### 1A. Archived-package functions (KEEP — reference/validation)

These are intentionally included from archived CRAN packages or GitHub repos. They carry license headers and serve as validation references. **Do not rename or refactor these** — they are historical and should stay as-is.

| Function | Source file | Origin | License |
|---|---|---|---|
| `williamsTest_JG()` | `TrendTest_JG.R` | StatCharrms (archived) | CC0 |
| `getLineContrast()` | `TrendTest_JG.R` | StatCharrms (archived) | CC0 |
| `getQuadContrast()` | `TrendTest_JG.R` | StatCharrms (archived) | CC0 |
| `monotonicityTest()` | `TrendTest_JG.R` | StatCharrms (archived) | CC0 |
| `getwilliamRes()` | `StatCharrms.R` | StatCharrms (archived) | CC0 |
| `runRSCABS()` | `RSCABS.R` | RSCABS (archived) | CC0 |
| `stepDownRSCABS()` | `RSCABS.R` | RSCABS (archived) | CC0 |
| `stepKRSCABS()` | `RSCABS.R` | RSCABS (archived) | CC0 |
| `prepDataRSCABS()` | `RSCABS.R` | RSCABS (archived) | CC0 |
| `RSCABK()` | `RSCABS.R` | RSCABS (archived) | CC0 |
| `tsk()` / `tsk_auto()` | `brsr_tsk.R` | brsr/tsk (GitHub) | GPL-3 |
| `calcTaronesTest()` | `integrated_tarone.R` | ClinStats (GitHub) | MIT |
| `calpha.test()` | `integrated_calpha.R` | epiphy (archived) | MIT |

### 1B. Legacy camelCase functions (KEEP with snake_case aliases)

These are the package's own functions with camelCase names. Strategy: **add `snake_case` aliases, deprecate the old names gradually**.

| Current name | Proposed alias | Source file |
|---|---|---|
| `getEC50()` | `get_ec50()` | `Endpoints.R` |
| `getEndpoint()` | `get_endpoint()` | `Endpoints.R` |
| `getComparison()` | `get_comparison()` | `Endpoints.R` |
| `getModelName()` | `get_model_name()` | `Endpoints.R` |
| `addECxCI()` | `add_ecx_ci()` | `Endpoints.R` |
| `ED.plus()` | `ed_plus()` | `drc_Helper.R` |
| `ED.ZG()` | `ed_zg()` | `drc_Helper.R` |
| `mselect.ZG()` | `mselect_zg()` | `drc_Helper.R` |
| `mselect.ED()` | `mselect_ed()` | `drc_Helper.R` |
| `mselect.plus()` | `mselect_plus()` | `drc_Helper.R` |
| `summaryZG()` | `summary_zg()` | `drc_Helper.R` |
| `plot.modList()` | *(S3 method — keep)* | `drc_Helper.R` |
| `plot_edList()` | *(already snake — keep)* | `drc_Helper.R` |
| `drcCompare()` | `drc_compare()` | `drc_Helper.R` |
| `contEndpoint()` | `cont_endpoint()` | `continuous_tests.R` |
| `ECx_rating()` | *(mixed — keep)* | `Endpoints.R` |
| `simDRdata()` | `sim_dr_data()` | `dose_response_simulation.R` |
| `backCalcSE()` | `back_calc_se()` | `dose_response_simulation.R` |
| `calcNW()` | `calc_nw()` | `dose_response_simulation.R` |
| `calcSteepnessOverlap()` | `calc_steepness_overlap()` | `dose_response_simulation.R` |
| `cochranArmitageTrendTest()` | `cochran_armitage_trend_test()` | `stepdown_binom.R` |
| `stepDownTrendTestBinom()` | `step_down_trend_test_binom()` | `stepdown_binom.R` |
| `rankTransform()` | `rank_transform()` | `ordinal.R` |
| `convert2Score()` | `convert_to_score()` | `ordinal.R` |
| `pavaMean()` | `pava_mean()` | `ordinal.R` |
| `prelimPlot1/2/3()` | `prelim_plot_1/2/3()` | `prelimnary.R` |
| `prelimSummary()` | `prelim_summary()` | `prelimnary.R` |
| `simplifyTreatment()` | `simplify_treatment()` | `reshape_DRC_Data.R` |
| `logxp()` / `invlogxp()` | *(math — keep)* | `drc_Helper.R` |
| `SpearmanKarber_modified()` | `spearman_karber_modified()` | `SK_TSK_tests_wrapper.R` |
| `Tarone.test()` | `tarone_test()` | `overdispersion_binom.R` |
| `Tarone.trend.test()` | `tarone_trend_test()` | `overdispersion_binom.R` |

### 1C. Already snake_case (no changes needed)

`dunnett_test`, `dunn_test`, `compute_mdd_dunnett`, `compute_mdd_williams`, `broom_dunnett`, `broom_williams`, `compare_to_control_fisher`, `compare_to_control_welch`, `noec_from_trend_test`, `step_down_RSCABS`, `run_RSCA`, `run_threshold_RSCA`, `run_all_threshold_tests`, `mqjt_test`, `step_down_jt`, `analyze_SK`, `calculate_noec_rstatix`, `test_overdispersion`, `simulate_dose_response`, `expand_to_individual_simple/tidy`, `aggregate_from_individual_simple/tidy`, `create_contingency_table`, `many_to_one_fisher_test`, `convert_fish_data`, `report_dunnett_summary`, `oscillating_response`, `log_message`, `stepDownTrendTest_NOEC`, `get_CA_Z`, `get_RS_adj_val`, `dose.p.glmmPQL`, `reshape_drcData`, `treatment2dose`, `complete_trend_analysis`, `compare_phi_methods`, `comprehensive_phi_comparison`, `compare_tarone_scoring`, `estimate_phi_with_scoring`, `create_summary_table`

---

## 2. Pipe Migration: `%>%` → `|>`

### Risk Assessment

- The native pipe `|>` requires **R >= 4.1** (released June 2021)
- Current DESCRIPTION says `R >= 2.10` which is far too old anyway
- Practically nobody runs R < 4.1 in 2026

### Migration Strategy (Safe, Incremental)

**Phase 0 — Bump R version requirement**
- Change `Depends: R (>= 4.1.0)` in DESCRIPTION
- This is the prerequisite; `|>` won't parse on older R

**Phase 1 — Stop re-exporting `%>%`**
- Remove `utils-pipe.R` (the magrittr re-export)
- Remove `magrittr` from Imports
- Remove `importFrom(magrittr, "%>%")` and `importFrom(dplyr, "%>%")` from NAMESPACE/roxygen
- Run `devtools::document()` + `devtools::check()`

**Phase 2 — Mechanical replacement in package code**
- Find-and-replace `%>%` → `|>` in all `R/*.R` files
- **Exception**: Do NOT touch archived-package files (`RSCABS.R`, `StatCharrms.R`, `TrendTest_JG.R`, `brsr_tsk.R`, `integrated_tarone.R`, `integrated_calpha.R`) unless they use `%>%` (most don't)
- Run tests after each file

**Phase 3 — Update tests and vignettes**
- Replace `%>%` in `tests/testthat/*.R`
- Replace `%>%` in `vignettes/*.Rmd`
- Replace in `README.Rmd`

**Phase 4 — Update documentation**
- Remove `%>%` from `@examples` in roxygen
- Update any `@importFrom dplyr %>%` tags
- Run `devtools::document()` + full `devtools::check()`

### Files with `%>%` to convert (package code only)

| File | Approx. pipe uses |
|---|---|
| `R/NOEC.R` | 6 |
| `R/MDD.R` | 5 |
| `R/overdispersion_binom.R` | 6 |
| `R/ordinal.R` | 8 |
| `R/quantal_tests.R` | 10 |
| `R/prelimnary.R` | 5 |
| `R/stepdown_binom.R` | 7 |
| `R/RSCABS_AO.R` | 8 |
| `R/reshape_DRC_Data.R` | 3 |
| `R/broom.R` | 2 (in `@importFrom` tags) |
| `R/drc_Helper.R` | 5 (mostly in `@examples`) |

---

## 3. Redundancy Consolidation (from REDUNDANCY_ANALYSIS.md)

### Already done
- [x] Identified RSCABS duplication
- [x] Removed empty `MQJT.R` placeholder → **Wait, MQJT.R has 439 lines now**
- [x] Added deprecation warnings to `runRSCABS()`

### Note on MQJT.R
The redundancy analysis says it was removed (3-line placeholder), but it currently has 439 lines with `mqjt_test()`, `print.mqjtTest()`, `summary.mqjtTest()`. This appears to be a **new addition** that was added after the redundancy analysis was written. It's not redundant — it's a new feature.

### Remaining consolidation work
- [ ] Add deprecation warnings to legacy RSCABS functions (point to `step_down_RSCABS()`)
- [ ] Document that `StatCharrms.R` functions are reference-only (add `@note`)
- [ ] Consider whether `Tarone.test()` / `Tarone.trend.test()` should be aliased under `test_overdispersion()`

---

## 4. File-Level Cleanup

| Issue | Action |
|---|---|
| `wiiliams_JT.R` — typo in filename | Rename to `williams_JT.R` (use `git mv`) |
| `prelimnary.R` — typo in filename | Rename to `preliminary.R` (use `git mv`) |
| `data_Helper.R` — mixed case | Rename to `data_helper.R` |
| `reshape_DRC_Data.R` — mixed case | Rename to `reshape_drc_data.R` |
| `R >= 2.10` in DESCRIPTION | Bump to `R >= 4.1.0` |

---

## 5. Recommended Execution Order

1. **Bump R version** to `>= 4.1.0` — low risk, enables everything else
2. **Rename files** with typos — cosmetic but reduces confusion
3. **Add snake_case aliases** — non-breaking, just adds new exports
4. **Migrate pipes** `%>%` → `|>` — mechanical, test after each file
5. **Remove magrittr** dependency — after pipe migration is complete
6. **Add deprecation warnings** to camelCase names — after aliases are stable
7. **Update REDUNDANCY_ANALYSIS.md** — reflect current state
8. **Update vignettes and README** — last, after code is settled

---

## 6. Snake_case Alias Pattern

Use this pattern to add aliases without breaking existing code:

```r
#' @rdname getEC50
#' @export
get_ec50 <- getEC50
```

And add a deprecation warning to the old name in a later phase:

```r
#' @export
getEC50 <- function(...) {
  .Deprecated("get_ec50")
  get_ec50(...)
}
```
