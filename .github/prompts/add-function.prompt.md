---
description: Add a new exported function to the drcHelper R package
---

# Add New Function

Add a new exported function to the drcHelper package following the standard workflow.

## Steps

1. Read `.github/memory/ARCHITECTURE.md` and `.github/memory/CONVENTIONS.md` for context
2. Create or edit the appropriate `R/*.R` file
3. Write roxygen2 documentation with `@param`, `@return`, `@export`, `@family`, `@examples`
4. Run `devtools::document()` to update NAMESPACE and man pages
5. Create `tests/testthat/test-{function_name}.R` with `describe()`/`it()` blocks
6. Run `devtools::test(filter = "{function_name}")` to verify tests pass
7. Update `NEWS.md` with the new function entry
8. Update `.github/memory/PROGRESS.md` with what was added

## Function: {{function_name}}
## Purpose: {{purpose}}
## Family: {{family_tag}}
