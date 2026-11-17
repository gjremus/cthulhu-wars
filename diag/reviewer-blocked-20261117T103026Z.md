# Reviewer run BLOCKED (3rd consecutive) — private coordination repo still unreachable

**UTC:** Tue Nov 17 10:30:26 UTC 2026
**Branch:** diag/reviewer-blocked-20261117T103026Z
**Session:** https://claude.ai/code/session_013LyCWdDKFYrRWN8SizKrt4

## Status
Identical blocker to the 08:31 and 09:30 wakes (see
`diag/reviewer-blocked-20261117T083100Z.md` for the full diagnosis). The private
coordination repo `gjremus/cthulhu-wars-pipeline` is still unreachable.

Re-verified this run:
- `git clone -b pipeline https://.../cthulhu-wars-pipeline` -> `could not read
  Username` (public code repo clones/fetches fine anonymously; so auth, not network).
- GitHub MCP `get_file_contents` on the pipeline repo -> "Access denied:
  repository not configured for this session. Allowed repositories:
  gjremus/cthulhu-wars." No `add_repo`/`list_repos` tool exposed to this wake.
- Public repo read + write confirmed working this run (this diag branch pushed).

## Impact
The entire pipeline bus (inbox/, deploy/, wip/, results/, updates/, questions/)
lives in the private repo, so all four stages (A deploy sweep, B deploy results,
C triage, D build) are blocked. **Three consecutive hourly reviewer runs have now
produced no pipeline work.** Any pending submissions/builds are stalled.

## Not self-healing — human action required (one of):
1. Allow a git credential helper (`gh auth setup-git` / `gh auth token`) for the
   scheduled reviewer wake, OR
2. Have the egress proxy inject GitHub auth for both repos, OR
3. Add `gjremus/cthulhu-wars-pipeline` to this session's GitHub MCP scope
   (read + write).

Chaining further wakes is pointless until one of the above is done, so this run
does NOT re-fire. A phone/email notification was sent this run to surface the
recurrence.
