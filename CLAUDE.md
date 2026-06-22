# nelisp-cfront — Claude work rules

> Read in addition to the root `Notes/CLAUDE.md` and `dev/CLAUDE.md`.

## Existence purpose

C front-end for the NeLisp toolchain: lower C source onto the
`nelisp-cc` grammar (which `nelisp` AOT-compiles to native). This repo
is the *front-end*; `nelisp-cc` is the *back-end*. The wider charter
(pure-elisp化 / Rust LOC reduction) is served because a working C→grammar
path is the same substrate `nelisp` is already building for Doc 122-128.

Read first:

- `README.org`
- `docs/design/01-c-to-nelisp-cc-lowering.org`

## Operating principles

- This is a **feasibility spike first**. Do not build the C99 parser
  before the lowering/runtime fit is proven. Hand-lower C to grammar
  sexps; isolate the *unknown* (does C semantics fit the grammar) from
  the *known-doable* (parsing C).
- Lowering target is the **real `nelisp-cc` grammar** (native compile),
  not interpreted elisp emulation. Verify every grammar op against the
  `nelisp` source before relying on it — do not assume an op exists.
- Depend on `nelisp` via load-path (`NELISP_REPO_ROOT`, default
  `../nelisp`). Never fork/vendor nelisp.
- Honest status: leaf ops (`ptr-read-u64` …) currently route through
  Rust externs in nelisp; full pure-elisp end-to-end is gated on the
  nelisp Doc 122-128 leaf migration. State this rather than overclaim.

## Worklog / memory

- Follow `AGENTS.md`: record work via `anvil-worklog` (MCP `worklog-add`),
  never as repo handoff files.
