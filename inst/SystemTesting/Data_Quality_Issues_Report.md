# Dunnett Test Data Quality Issues Report

**Date:** September 23, 2025  
**Prepared for:** Test Data Provider  
**Analysis:** drcHelper Package Validation

## Executive Summary

The Dunnett test validation revealed **critical data quality issues** in the test datasets that prevent proper validation. While the validation framework is working correctly, the reference data contains systematic errors that need immediate attention.

## Critical Issues Identified

### 1. **Missing/Invalid Expected Values (605 rows affected)**
- **Problem:** 605 rows in `test_cases_res` contain invalid placeholders (`"-"`, `"NA"`, empty values)
- **Impact:** Cannot validate statistical computations when reference values are missing
- **Example:** T-statistic expected values show `"-"` instead of numeric values

### 2. **Data Misalignment in Expected Results**
**Study:** MOCK08/15-001 (Aphidius Repellency, FG00222, two-sided test)

| Issue | Details |
|-------|---------|
| **Missing T-statistics** | Doses 0.2 and NA have missing expected T-values |
| **Mean value misalignment** | Expected means are assigned to wrong doses |
| **Inconsistent NA handling** | Some doses show NA, others show "-" for missing data |

### 3. **Specific Data Errors Found**

#### Expected vs Actual Mean Comparison (MOCK08/15-001 Repellency):
```
Dose   | Expected Mean | Actual Mean | Status
-------|---------------|-------------|--------
0      | -            | 33.50       | Missing expected
0.1    | 27.94        | 62.44       | MISMATCH (34.5 difference!)
0.2    | 33.5         | 37.17       | Slight difference
0.3    | 37.17        | 52.89       | MISMATCH (15.7 difference!)
0.375  | 52.89        | 53.44       | Match
0.625  | 53.44        | 29.50       | MISMATCH (23.9 difference!)
2      | 29.5         | 27.94       | Slight difference
```

**Root Cause:** The expected means appear to be shifted/rotated relative to the actual dose assignments.

#### T-statistic Issues:
- Dose 0.2: Expected = `NA`, should have a numeric value
- Dose NA: Expected = `"-"`, invalid placeholder
- Other doses have numeric values but may be misaligned due to mean errors

## Impact on Validation

- **Mean validations:** All failing due to dose misalignment
- **T-statistic validations:** Failing due to mean misalignment propagation
- **P-value validations:** Generally passing (less sensitive to small mean differences)
- **Overall test result:** FAIL due to systematic data errors

## Required Actions

### Immediate (High Priority)
1. **Fix dose-mean alignment** in `test_cases_res` for all studies
2. **Replace all `"-"` placeholders** with proper numeric values or valid NA
3. **Verify T-statistic calculations** are based on corrected means
4. **Standardize missing value representation** (use R's `NA`, not `"-"` or `"NA"`)

### Medium Priority
5. **Cross-validate all expected results** against independent calculations
6. **Implement data integrity checks** in the data preparation process
7. **Document expected value calculation methodology**

## Validation Framework Status

✅ **The validation framework is working correctly**
- Successfully identifies data quality issues
- Provides detailed expected vs actual comparisons
- Shows exact differences and tolerances
- Properly handles different test alternatives (less, greater, two.sided)

The validation failures are **data quality issues**, not framework bugs.

## Next Steps

1. **Data Provider:** Fix the identified data alignment and missing value issues
2. **drcHelper Team:** Re-run validation after data corrections
3. **Documentation:** Update with corrected validation results

---

**Files for Reference:**
- Current validation report: `Dunnett_Test_Cases_Original_Data_Issues.html`
- Data files: `test_cases_data.rda`, `test_cases_res.rda`