---
description: End a work session and persist context for future sessions
---

# End Session

Save the current work state so the next chat session can bootstrap quickly.

## Steps

1. Summarize what was accomplished in this session
2. Update `.github/memory/PROGRESS.md`:
   - Add completed items to "Recent Changes"
   - Update "Next Steps" with remaining work
   - Update "Known Issues" if new ones were discovered
3. If any design decisions were made, add them to `.github/memory/DECISIONS.md` with:
   - Date
   - Decision
   - Rationale
   - Impact
4. If the architecture changed (new modules, reorganized files), update `.github/memory/ARCHITECTURE.md`
5. If new coding patterns were established, update `.github/memory/CONVENTIONS.md`
6. Stage and commit the memory files
