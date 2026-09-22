---
name: libraries
description: Discover and reuse Logtalk standard libraries instead of reimplementing predicates. Use when writing utilities, working with lists, sets, dictionaries, types, OS, random, JSON, or any common infrastructure; or when tempted to add a local helper that may already exist.
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

## Context7

When Context7 (CLI, skill, or MCP) is configured:

1. Resolve Logtalk / the relevant library id.
2. Query with a concrete topic (predicate name, library name, or task).
3. Apply the returned API; do not fall back to inventing helpers if the docs list one.

If Context7 is unavailable, use the official API index and handbook links above.

## What to avoid

- Local `member/2`, `append/3`, `reverse/2`, `map`-style helpers that duplicate libraries
- New “utils" objects that mirror standard library APIs without need
- Guessing predicate names or arities instead of looking them up
- Loading libraries only in interactive sessions and forgetting them in `loader.lgt`
- Copying large documentation blocks into source files

## Verification checklist

- [ ] Searched APIs / Context7 before adding any general-purpose helper
- [ ] Required libraries listed in the appropriate `loader.lgt`
- [ ] Application code uses `uses/2` or documented public APIs
- [ ] No unjustified duplicate of a library predicate
- [ ] Backend-portable usage unless the library itself is backend-specific and documented as such

## Further reading

- https://logtalk.org/apis/index.html
- https://logtalk.org/handbook/libraries/index.html
- Application `loader.lgt` patterns in the coding-style guidelines and handbook
- Context7: https://github.com/upstash/context7 (setup via `ctx7 setup` or MCP as appropriate)
