---
description: Add tests for an existing function in drcHelper
---

# Add Tests

Write comprehensive tests for an existing function in the drcHelper package.

## Steps

1. Read the function source code in `R/` to understand inputs, outputs, and edge cases
2. Read `.github/memory/CONVENTIONS.md` for testing patterns
3. Create or update `tests/testthat/test-{function_name}.R`
4. Use `describe()`/`it()` blocks covering:
   - Typical usage with expected output
   - Edge cases (empty input, single row, NA values)
   - Error conditions (invalid types, missing columns)
   - Boundary conditions
5. Run `devtools::test(filter = "{function_name}")` to verify
6. Check coverage with `devtools::test(filter = "{function_name}")` if needed

## Function to test: {{function_name}}
## Source file: {{source_file}}
