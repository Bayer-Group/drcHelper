---
description: Review and fix R CMD check issues in drcHelper
---

# Fix R CMD Check

Diagnose and fix issues from `devtools::check()`.

## Steps

1. Run `devtools::check()` and capture output
2. Categorize issues: ERROR, WARNING, NOTE
3. For each issue:
   - Identify the root cause
   - Apply the fix
   - Re-run relevant tests
4. Common fixes:
   - **Undocumented arguments**: Add missing `@param` in roxygen
   - **Undefined global variables**: Add `.data` pronoun or `utils::globalVariables()`
   - **Missing imports**: Add `@importFrom` or use `pkg::fun()` syntax
   - **Missing Suggests**: Add package to DESCRIPTION with `usethis::use_package("pkg", "Suggests")`
   - **Examples errors**: Wrap slow examples in `\donttest{}`
5. Run `devtools::check()` again to confirm clean
6. Update `.github/memory/PROGRESS.md` with fixes applied
