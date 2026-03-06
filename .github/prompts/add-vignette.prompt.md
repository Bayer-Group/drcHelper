---
description: Write or update a vignette for the drcHelper package
---

# Add or Update Vignette

Create or modify a vignette (long-form documentation) for the drcHelper package.

## Steps

1. Read `.github/memory/ARCHITECTURE.md` for the relevant module context
2. Create vignette with `usethis::use_vignette("{{topic}}")` or edit existing file in `vignettes/`
3. Structure the vignette:
   - Introduction and motivation
   - Setup (loading packages, data)
   - Step-by-step walkthrough with code chunks
   - Interpretation of results
   - References
4. Use `devtools::build_vignettes()` to verify it renders
5. For pkgdown articles (not shipped with package), place in `vignettes/articles/`
6. Update `_pkgdown.yml` if adding to the site navigation

## Topic: {{topic}}
