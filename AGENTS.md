# Agent Instructions

Follow the parent `../AGENTS.md` worklog policy strictly.

For this repository:

- Do not add `.org`, `.md`, or `.txt` worklog handoff files to the repo.
  Record work through `anvil-worklog` only (MCP `worklog-add`, or the
  local `nelisp` command as fallback — not `emacsclient`).
- Design notes belong in `docs/design/NN-*.org` (numbered design docs),
  not as ad-hoc handoff files.
- This is a *feasibility spike*. Prefer hand-lowered C→grammar probes
  with ERT assertions over building the C99 front-end prematurely. The
  unknown being measured is the *semantic/runtime fit*, not whether a C
  parser can be written.
- Depends on the sibling `nelisp` repo. Do not vendor or fork it; resolve
  it via load-path (`NELISP_REPO_ROOT`, default `../nelisp`).
