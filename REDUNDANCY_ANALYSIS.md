# drcHelper Package Redundancy Analysis

## Overview
This document identifies structural redundancy issues in the drcHelper package and provides recommendations for consolidation.

## Identified Redundancy Issues

### 1. RSCABS Implementation Duplication (HIGH PRIORITY)

**Problem**: Two different implementations of RSCABS (Rao-Scott adjusted Cochran-Armitage trend test by slices):

- **Legacy Implementation** (`R/RSCABS.R`, 386 lines):
  - `runRSCABS()` - Main function from archived StatCharrms package
  - `stepDownRSCABS()` - Internal step-down procedure
  - `stepKRSCABS()` - Internal helper
  - Documented as "for validation purpose"
  - Uses older API design

- **Current Implementation** (`R/RSCABS_AO.R`, 934 lines):
  - `step_down_RSCABS()` - Modern step-down procedure
  - `run_RSCA()` - Basic RSCA test
  - `run_threshold_RSCA()` - Threshold-specific RSCA
  - `run_all_threshold_tests()` - Comprehensive testing
  - By Allen Olmstead, more modern design

**Impact**: 
- API confusion with similar function names
- Maintenance burden of two codebases
- Test suite covers both implementations

**Recommendation**: 
- Phase out legacy implementation gradually
- Update tests to use modern API
- Add deprecation warnings to legacy functions

### 2. Large Monolithic Files (MEDIUM PRIORITY)

**Problem**: Several files exceed 500+ lines with mixed responsibilities:

- `R/RSCABS_AO.R` (934 lines): Statistical functions + S3 methods + plotting
- `R/stepdown_binom.R` (825 lines): Multiple test procedures + utilities  
- `R/drc_Helper.R` (870 lines): Mixed DRC utilities

**Impact**:
- Difficult to navigate and maintain
- Mixed concerns in single files
- Harder code review process

**Recommendation**:
- Break into logical modules:
  - Core statistical functions
  - S3 methods and printing
  - Plotting functions
  - Utility functions

### 3. Statistical Test Organization

**Current State**: Statistical tests are scattered across multiple files:
- Tarone tests: `integrated_tarone.R`, `overdispersion_binom.R`
- Trend tests: `TrendTest_JG.R`, `stepdown_binom.R`
- Cochran-Armitage: `stepdown_binom.R`, `RSCABS_AO.R` (different purposes)

**Note**: Some apparent "duplication" serves different purposes:
- `cochranArmitageTrendTest()` - General-purpose public API
- `get_CA_Z()` - Internal utility for clustered data
- These are NOT redundant

## Consolidation Plan

### Phase 1: Documentation and Planning
- [x] Create this analysis document
- [ ] Add deprecation warnings to legacy functions
- [ ] Document migration path from old to new API

### Phase 2: API Consolidation  
- [ ] Update tests to prefer modern RSCA implementation
- [ ] Add wrapper functions for backward compatibility
- [ ] Mark legacy functions as deprecated in documentation

### Phase 3: File Organization
- [ ] Split large files into logical modules
- [ ] Group related statistical functions
- [ ] Separate S3 methods into dedicated files

### Phase 4: Cleanup
- [ ] Remove deprecated functions (with major version bump)
- [ ] Consolidate similar utility functions
- [ ] Update documentation to reflect new structure

## Files Changed
- Added: `REDUNDANCY_ANALYSIS.md`
- Removed: `R/MQJT.R` (empty placeholder file)

## Backward Compatibility
All changes maintain backward compatibility. Legacy functions remain available but should be considered deprecated for new code.