# Reviewer run BLOCKED (recurrence) — private coordination repo still unreachable

**UTC:** Tue Nov 17 09:30:45 UTC 2026
**Branch:** diag/reviewer-blocked-20261117T093045Z
**Session:** https://claude.ai/code/session_013LyCWer9bJTr949j6Wke3F

## Status
Same blocker as the 08:31 wake (see `diag/reviewer-blocked-20261117T083100Z.md`
for the full diagnosis). The private coordination repo
`gjremus/cthulhu-wars-pipeline` is still unreachable from this scheduled wake.

Re-verified this run:
- `git clone -b pipeline https://.../cthulhu-wars-pipeline`
  -> `could not read Username` (public repo clones fine; so auth, not network).
- Proxy `/__agentproxy/status`: `gitConfigInjection:true` but only sets
  insteadOf + `credential.interactive=false` — no GitHub auth token injected.
  Direct `curl` to the private repo's `info/refs` returns **401**; the public
  repo returns **200** anonymously.
- GitHub MCP `get_file_contents` on the pipeline repo -> "Access denied:
  repository not configured for this session. Allowed repositories:
  gjremus/cthulhu-wars." No `add_repo`/`list_repos` tool exposed to this wake.

## Impact
The pipeline bus (inbox/, deploy/, wip/, results/, updates/, questions/) lives
entirely in the private repo, so **all four stages (A deploy sweep, B deploy
results, C triage, D build) are blocked**. No deploy signal, triage, or build
progress was produced this run — now at least 2 consecutive hourly runs down.

## This is not self-healing
Retrying will not fix it. One of the unblock options from the 08:31 diag is
required: allow a git credential helper (`gh auth setup-git`) for scheduled
reviewer wakes, OR have the egress proxy inject GitHub auth for both repos, OR
add `gjremus/cthulhu-wars-pipeline` to this session's GitHub MCP scope with
read/write. Chaining to another wake is pointless until one of these is done.
