# Design Decisions — drcHelper Package

> **Purpose**: Log of significant design and implementation decisions. Update when new decisions are made.

## Decision Log

### 2026-03-06 — Established AI memory system

- **Decision**: Created `.github/memory/` with `ARCHITECTURE.md`, `DECISIONS.md`, `PROGRESS.md`, `CONVENTIONS.md`
- **Rationale**: Copilot chat sessions are stateless; persisting key context in files lets new sessions bootstrap quickly without re-reading the entire codebase
- **Impact**: Each new chat session should read these files first

### 2026-03-06 — Switch from magrittr `%>%` to native `|>`

- **Decision**: Migrate all package code from `%>%` to `|>`, bump R requirement to `>= 4.1.0`
- **Rationale**: R 4.1 is 5 years old (2021); native pipe has no dependency cost, is now the tidyverse standard, and simplifies NAMESPACE
- **Impact**: Removes `magrittr` dependency; breaks compatibility with R < 4.1 (acceptable); must update ~70+ pipe sites, tests, vignettes, and examples

### 2026-03-06 — snake_case aliases for legacy camelCase exports

- **Decision**: Add `snake_case` aliases for all camelCase exported functions; later deprecate the camelCase names
- **Rationale**: Package convention is `snake_case` but ~30 legacy exports use `camelCase`. Direct rename would break users. Alias-then-deprecate is safe.
- **Impact**: Temporarily doubles exports; deprecation warnings added in a later phase

### 2026-03-06 — Archived-package functions are NOT redundant

- **Decision**: Functions copied from archived CRAN packages (StatCharrms, RSCABS, tsk, ClinStats, epiphy) are kept as-is for validation/reference. Do not rename, refactor, or count as redundant.
- **Rationale**: These carry license headers (CC0, MIT, GPL-3) and serve as reference implementations against which the package's own implementations can be validated
- **Impact**: These files are excluded from naming standardization and pipe migration

### Package-level decisions (pre-existing)

#### Use `drc` fork from DoseResponse org
- **Decision**: Depend on `DoseResponse/drc` and `DoseResponse/drcData` via `Remotes`
- **Rationale**: Upstream CRAN `drc` has issues that are fixed in the fork
- **Impact**: Users must install from GitHub (`devtools::install_github()` or `pak::pak()`)

#### `testthat` edition 3 with `describe()`/`it()` style
- **Decision**: Use BDD-style test blocks
- **Rationale**: More readable for domain-specific logic; aligns with how statistical test expectations are phrased

#### magrittr pipe over native pipe (SUPERSEDED — see 2026-03-06 native pipe decision)
- **Decision**: Use `%>%` throughout
- **Rationale**: Package supports R >= 2.10; native pipe requires R >= 4.1

#### Broom-style tidiers for statistical tests
- **Decision**: Created `broom.R` with tidy/glance methods for Dunnett, Williams results
- **Rationale**: Consistent, pipeable output format for downstream use

---

*Add new decisions above this line, with date and the four fields: Decision, Rationale, Impact.*
