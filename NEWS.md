# drcHelper 0.0.4.9001()

* Initial CRAN submission preparation.

## New Features
- Added `tsk_auto` for trimmed spearman-karber LD50 calculation.
- Added `SpearmanKarber_modified` for Spearman-Karber LD50 calculation.
- Added `compare_to_control_welch` for Welch's t-test with multiple comparison
- Added `compute_mdd_williams` and `compute_mdd_dunnett` for MDD calculations, `report_dunnett_summary` for the reporting of test result.
- Added many articles to explain concepts and verify implementations in `drcHelper`

## Improvments
- Added test cases validation reports. 
  - `/inst/SystemTesting/Consolidated_Dunnett_Report.Rmd`

## Bug Fixes
- Updated `dunnett_test` to correctly handle non-RCBD replicate IDs.

