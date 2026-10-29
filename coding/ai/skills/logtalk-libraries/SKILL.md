---
name: logtalk-libraries
description: Discover and reuse Logtalk standard libraries instead of reimplementing predicates. Use when writing utilities, working with lists, sets, dictionaries, types, OS, random, JSON, or any common infrastructure; or when tempted to add a local helper that may already exist.
license: Apache-2.0
metadata:
  copyright: "2026 Paulo Moura <pmoura@logtalk.org>"
---

# Libraries Skill

Prefer Logtalk’s standard libraries over local reimplementations. Before writing a utility predicate, look up the existing API and load the library.

## Authoritative documentation

- API index: https://logtalk.org/apis/index.html
- Libraries handbook: https://logtalk.org/handbook/libraries/index.html

Do not paste large API extracts into source files or comments. Link or use `uses/2` instead.

## When to use this skill

- Implementing list, set, dictionary, type-check, OS, random, timeout, JSON, or similar helpers
- Reviewing code that defines `member/2`, `append/3`, `map/3`, `length/2`, etc. locally
- Choosing how to load and import library predicates into an application entity
- Using Context7 / `ctx7` (or equivalent) to fetch current Logtalk library docs

## Mandatory workflow

1. **Search before you write**
   - Check https://logtalk.org/apis/index.html (and the libraries handbook) for an existing predicate or entity.
   - Do not assume common Prolog library predicate names are used: look into the predicate descriptions for the sought functionality.
   - If Context7 is available:
     - Resolve the library (e.g. `ctx7 library logtalk` or MCP `resolve-library-id`).
     - Query docs for the topic (e.g. `ctx7 docs <libraryId> "lists member"` or MCP `query-docs`).
   - Only implement a local predicate when no suitable library API exists or semantics must differ (document why).

2. **Load the library**
   - Typical form: `logtalk_load(lists(loader))` (replace `lists` with the library name).
   - Prefer loading libraries from the application or library `loader.lgt`, not ad-hoc in every file.
   - Load external library dependencies using library notation rather than relative filesystem paths.
   - If a change requires updated dependencies to be installed or reloaded, stop and ask the user to install/reload them; do not add workaround wrappers.

3. **Use, don’t copy**
   - Prefer `uses/2` for readable calls:
     ```logtalk
     :- uses(list, [
         member/2, append/3, length/2
     ]).
     ```
   - Or send messages / use the library’s public API as documented.
   - Follow existing Logtalk patterns (protocols, parametric objects, categories) when extending behavior.

4. **Verify**
   - Code compiles and runs with the library loaded.
   - No parallel local predicate with the same role as a documented library predicate without an explicit reason.

## Common libraries (starting points)

Names and loaders are indicative; confirm in the API index for your Logtalk version.

| Area | Examples (library / loader pattern) |
|------|-------------------------------------|
| Lists, sets, sequences | `lists`, related collection libraries |
| Dictionaries, heaps | dictionary / heap libraries in the distribution |
| Types and checks | `types` (and related type-checking support) |
| OS / files / paths | `os` and related |
| Random | `random` and protocol implementations |
| JSON, CSV, XML, etc. | corresponding libraries when present |
| Meta / higher-order | meta-predicate libraries and lambdas |
| Timeouts, logging, … | see full library index |

Always prefer the exact entity and predicate names from the official docs over guessed names.

## Shared API contracts

- Follow the shared `options` API: validate with `check_options/1`, merge with `merge_options/2`, accept repeated options, and use the first occurrence. Do not add duplicate rejection or fixed-size effective-option validators, including for options stored in models.
- Do not redeclare the `valid_option/1` and `default_option/1` predicates.
- An explicit options-list argument must be last, including in provider protocols.
- Do not introduce compatibility branches for new, unreleased libraries unless requested; enforce the complete current model representation.
- For forecasting data, represent missing observations with anonymous or unbound variables (`_`), not configurable atoms or compound markers.
- In descriptor DSLs that generate JSON, use ordinary `true`/`false` in descriptors and reserve `@true`/`@false` for generated JSON. In curly JSON terms, use a space before literals on the right of a pair: `flag- @true`, `value- @null`.
- Use `create_object/4` for dynamic object creation; group protocols in one `implements([Protocols])` relation.
- `current_predicate/1` sees protocol declarations. Use `predicate_property(Head, defined_in(Entity))` when distinguishing declaration from implementation is necessary; avoid redundant checks for declared optional hooks whose calls already fail cleanly when undefined.
- When an API accepts either a compound handle or an integer port, discriminate by structure or exclude integers; `nonvar/1` alone hides the port branch.

## Context7

When Context7 (CLI, skill, or MCP) is configured:

1. Resolve Logtalk / the relevant library id.
2. Query with a concrete topic (predicate name, library name, or task).
3. Apply the returned API; do not fall back to inventing helpers if the docs list one.

If Context7 is unavailable, use the official API index and handbook links above.

## What to avoid

- Local `member/2`, `append/3`, `reverse/2`, `map`-style helpers that duplicate libraries
- New "utils" objects that mirror standard library APIs without need
- Guessing predicate names or arities instead of looking them up
- Loading libraries only in interactive sessions and forgetting to add them to `loader.lgt` and `tester.lgt` files
- Copying large documentation blocks into source files

## Verification checklist

- [ ] Searched APIs / Context7 before adding any general-purpose helper
- [ ] Required libraries listed in the appropriate `loader.lgt` and `tester.lgt` files
- [ ] Application code uses `uses/2` or documented public APIs
- [ ] No unjustified duplicate of a library predicate
- [ ] Backend-portable usage unless the library itself is backend-specific and documented as such

## Further reading

- https://logtalk.org/apis/index.html
- https://logtalk.org/handbook/libraries/index.html
- Application `loader.lgt` patterns in the coding-style guidelines and handbook
- Context7: https://github.com/upstash/context7 (setup via `ctx7 setup` or MCP as appropriate)
