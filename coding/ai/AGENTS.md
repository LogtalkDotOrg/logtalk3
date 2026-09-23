# AGENTS.md — Logtalk applications

Guidance for AI coding agents working on Logtalk code (applications, libraries, or the Logtalk distribution itself).

Logtalk is a declarative object-oriented logic programming language that extends Prolog. Prefer Logtalk entities and idioms over raw Prolog style unless interfacing with plain Prolog or modules.

For task-specific procedures, load the matching skill under `coding-assistants/skills/` (or the copy in the application repo). Do not invent parallel frameworks for testing, documentation, or debugging.

## Essential docs (do not duplicate)

- Handbook: https://logtalk.org/handbook/index.html
- Coding style: https://logtalk.org/coding_style_guidelines.html
- Testing (`lgtunit`): https://logtalk.org/handbook/devtools/lgtunit.html
- Documenting: https://logtalk.org/handbook/userman/documenting.html
- Debugging: https://logtalk.org/handbook/userman/debugging.html
- Libraries overview and descriptions: https://logtalk.org/handbook/libraries/index.html
- APIs / libraries: https://logtalk.org/docs/index.html

## Skills (on demand)

| Skill | Use when |
|-------|----------|
| `skills/coding-style/SKILL.md` | Layout, naming, refactors, style review |
| `skills/debugging/SKILL.md` | Debug mode, debugger, breakpoints, tracing |
| `skills/documenting/SKILL.md` | `info/1`, `info/2`, `mode/2`, `lgtdoc` |
| `skills/libraries/SKILL.md` | Reusing standard libraries; avoiding duplicate local helpers |
| `skills/packs/SKILL.md` | Authoring packs and registries (`pack_protocol`, versions, checksums) |
| `skills/performance/SKILL.md` | Writing performant code |
| `skills/portability/SKILL.md` | Authoring portable applications and libraries |
| `skills/scaffolding/SKILL.md` | Scaffold a new app (directory, settings, loader, tester) |
| `skills/testing/SKILL.md` | Writing or running tests, `tester.lgt`, QuickCheck, coverage |

## Project layout (typical application or library)

- `loader.lgt` — loads code and dependencies
- `tester.lgt` — loads `lgtunit`, code under test, and tests; runs tests (name expected by `logtalk_tester`)
- `tests.lgt` (or `tests/`) — test objects extending `lgtunit`
- Optional: `doclet.lgt`, `NOTES.md`, `SCRIPT.txt`
- Single-entity file: name file after the entity; parametric entities append arity (e.g. `dict_1.lgt`)
- Extensions: `.lgt` or `.logtalk`
- Encoding: prefer US-ASCII or UTF-8

## Build, load, test

```logtalk
| ?- logtalk_load(loader).          % load application
| ?- logtalk_load(tester).          % load and run tests (typical driver)
| ?- tests::run.                    % re-run tests
| ?- tests::run(TestId).            % single test
```

Automated (from a directory containing `tester.lgt`):

```text
logtalk_tester -p <backend>
```

Compile tests with `hook(lgtunit)`. For coverage, compile code under test with `source_data(on)` and usually `debug(on)`. See the testing skill for dialects, parametric test objects, and CI notes.

## Coding style (summary)

Full rules: https://logtalk.org/coding_style_guidelines.html — also `skills/coding-style/SKILL.md`.

- **Indent with tabs only** (never mix tabs and spaces for indentation).
- **Predicates**: `snake_case`. **Variables**: `CamelCase`. Dynamic predicates often end with `_`.
- One goal per line in clause bodies (small exceptions: green cuts, `nl` after write).
- Parenthesize disjunctions and if-then-else; always include the else branch.
- Spaces around binary operators; space after commas and after `|` in lists.
- Prefer `info/1`, `info/2`, and `mode/2` over long unstructured comments.
- Do not use top-level load/`make` shortcuts inside source files.

## Entities and design

- Prefer protocols for interfaces; objects and categories for implementation and reuse.
- Use categories for fine-grained composition and hot patching when appropriate.
- Messages (`::/2`, `^^/1`) for object APIs; respect predicate scope (public/protected/private).
- Keep meta-arguments simple; prefer lambdas over ad-hoc auxiliaries when clarity allows.
- Write portable code; avoid backend-specific features unless isolated and documented.

## Standard libraries (do not reimplement)

- **Before writing a utility predicate, check whether it already exists** in the Logtalk libraries.
- Prefer library predicates over local duplicates (lists, sets, dictionaries, types, os, random, JSON, meta helpers, …).
- Load libraries explicitly, e.g. `logtalk_load(lists(loader))`, typically from the application `loader.lgt`.
- Import with `uses/2` (or the library’s documented public API); do not copy library clause bodies into application code.
- API index: https://logtalk.org/apis/index.html  
  Libraries overview and descriptions: https://logtalk.org/handbook/libraries/index.html
- When unsure of the exact predicate or entity, look it up (official APIs, handbook, or Context7 / `ctx7`) instead of inventing a parallel helper.
- Only reimplement when semantics must differ; document that reason in a short comment or `info/2`.
- Details and workflow: `skills/libraries/SKILL.md`

## Documenting

Use `info/1` (entities) and `info/2` (predicates/non-terminals), plus `mode/2` for public predicates. Generate API docs with `lgtdoc`. Details: documenting skill and handbook.

## Debugging

Compile entities in **debug mode** to trace them (`debug(on)`, `logtalk_make(debug)`, or `{+d}`). Load `debugger(loader)`. Use predicate/clause/conditional/context breakpoints via `debugger::spy/...`. Details: debugging skill and handbook.

## What not to do

- Do not replace `lgtunit` with a custom test harness.
- Do not skip `hook(lgtunit)` when compiling test objects.
- Do not put non-test entities in the same file as term-expanded tests.
- Do not leave debug compilation on for routine optimized runs.
- Do not duplicate handbook prose in generated code or comments; link instead.
- Do not invent non-standard documentation or style schemes.
- Do not reimplement standard library predicates locally without a documented reason.

## Verification before finishing a change

- [ ] Code loads via the project `loader.lgt`
- [ ] Required libraries are loaded from the loader (not reinvented in-app)
- [ ] Tests pass (`tester.lgt` / `logtalk_tester` / `tests::run`)
- [ ] New public APIs have `mode/2` and useful `info/2` where appropriate
- [ ] Style matches the summary above and neighboring code
- [ ] No backend-specific code without clear isolation
