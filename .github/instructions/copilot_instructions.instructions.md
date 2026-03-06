---
applyTo: '**/*.md'
---

# Markdown Documentation Guidelines — drcHelper

This instruction file applies to all Markdown files in the project.

## AI Agent Bootstrap

**At the start of every new chat session**, read these memory files to get full project context:

1. `.github/memory/ARCHITECTURE.md` — Package structure and module map
2. `.github/memory/DECISIONS.md` — Design decisions log
3. `.github/memory/PROGRESS.md` — Current work state and next steps
4. `.github/memory/CONVENTIONS.md` — Project-specific coding patterns

**Before ending a session** with significant work, update:
- `PROGRESS.md` — What was done, what's next
- `DECISIONS.md` — If any new design decisions were made

## Markdown Conventions

- Use ATX-style headings (`#`, `##`, `###`)
- Use fenced code blocks with language identifiers (````r`, ````bash`)
- Use reference-style links for URLs that appear multiple times
- Tables should use proper Markdown table syntax with alignment
- Keep line length reasonable (~100 chars) for readability in diffs

## R Package Documentation in Markdown

- `README.Rmd` is the source for `README.md` — edit the `.Rmd`, never the `.md` directly
- `NEWS.md` follows [Keep a Changelog](https://keepachangelog.com/) format
- Vignettes use `.Rmd` (R Markdown) with YAML front matter
- pkgdown articles go in `vignettes/articles/`

## Changelog Format (NEWS.md)

```markdown
# drcHelper X.Y.Z

## New Features
- Added `function_name` for [purpose] (#issue)

## Improvements
- Improved `function_name` to [change] (#issue)

## Bug Fixes
- Fixed `function_name` [what was wrong] (#issue)
```