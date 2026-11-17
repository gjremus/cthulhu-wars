# Reviewer run BLOCKED (4th consecutive) — private coordination repo still unreachable

**UTC:** Tue Nov 17 11:32:12 UTC 2026
**Branch:** diag/reviewer-blocked-20261117T113212Z
**Session:** https://claude.ai/code/session_013LyCWgHfnNZRcRkd8SbPKN

## Status
Same blocker as the 08:31, 09:30 and 10:30 wakes. The private coordination repo
`gjremus/cthulhu-wars-pipeline` remains unreachable.

Re-verified this run:
- `git clone -b pipeline https://.../cthulhu-wars-pipeline` -> `could not read
  Username for 'https://github.com': terminal prompts disabled`. Public code repo
  still clones fine anonymously, so this is auth, not network.
- Root cause confirmed this run: `gh auth status` reports **"The token in GH_TOKEN
  is invalid."** Proxy status shows `gitConfigInjection: true` but the injected
  config only rewrites SSH->HTTPS (`url.https://github.com/.insteadof`) and adds
  **no credential helper / extraheader**, so private-repo auth has nothing valid
  to present.
- GitHub MCP `get_file_contents` on the pipeline repo -> "Access denied:
  repository not configured for this session. Allowed repositories:
  gjremus/cthulhu-wars." No `add_repo` / `list_repos` tool exposed to this wake.
- Credential workarounds (SSH URL, creds-in-URL, inline credential helper) are
  blocked by the environment's command classifier, so they are not an option.
- Public repo read + write via GitHub MCP confirmed working this run (this diag
  branch + file created via MCP).

## Impact
The entire pipeline bus (inbox/, deploy/, wip/, results/, updates/, questions/)
lives in the private repo, so all four stages (A deploy sweep, B deploy results,
C triage, D build) are blocked. **Four consecutive hourly reviewer runs have now
produced no pipeline work.** Any pending submissions/builds are stalled.

## Not self-healing — human action required (one of):
1. Refresh the scheduled wake's GitHub token so `GH_TOKEN` is valid again and the
   proxy can inject auth (root cause is the invalid token), OR
2. Allow a git credential helper (`gh auth setup-git`) for the reviewer wake, OR
3. Add `gjremus/cthulhu-wars-pipeline` to this session's GitHub MCP scope
   (read + write) so the bus is reachable via MCP.

Chaining further wakes is pointless until one of the above is done, so this run
does NOT re-fire. The next hourly tick will re-check automatically. A phone/email
escalation notification was sent this run flagging the 4-hour outage.
