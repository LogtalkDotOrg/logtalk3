---
name: logtalk-portability
description: Write Logtalk code portable across Prolog backends. Use when implementing libraries or applications that must run on multiple systems, reviewing non-portable constructs, or isolating backend-specific code.
license: Apache-2.0
metadata:
  copyright: "2026 Paulo Moura <pmoura@logtalk.org>"
---

# Portability Skill

Write code that runs on any supported Prolog backend. Prefer Logtalk built-ins and ISO Prolog; isolate anything else behind a portable interface.

Portability across all supported backends is a hard requirement unless the user explicitly narrows it. Do not introduce backend-only solutions even as temporary experiments without that approval.

## Authoritative documentation

- Portable applications: https://logtalk.org/handbook/userman/programming.html#portable-applications
- Conditional compilation: same handbook page (following section)
- Compiler `portability` flag and linter: handbook compiler flags / linter sections

Do not duplicate the handbook. Link for detail.

## When to use this skill

- Writing or reviewing libraries, packs, or apps intended for multiple backends
- Seeing portability warnings from the Logtalk compiler
- Tempted to call a SWI-, SICStus-, YAP-, GNU-, or other backend-only predicate or library
- Using DCGs, atoms/strings, or flag-dependent syntax

## Hard rules (always)

1. **No proprietary Prolog features or libraries in portable code**  
   Do not call backend-only built-ins, modules, or libraries from shared application/library code.

2. **Do not depend on the Prolog flags `unknown` or `double_quotes`**  
   Behavior must not assume a particular value of these flags. Code that only works for one setting is non-portable.

3. **Do not use double-quoted terms in DCGs**  
   Avoid `"..."` terminals/non-terminals in grammar rules; their meaning varies with `double_quotes` and across systems. Prefer explicit representations (e.g. lists of character codes or chars via portable APIs, or Logtalk/library helpers) that do not rely on that flag.

4. **Prefer standard building blocks**  
   - Logtalk built-in predicates and methods  
   - ISO Prolog built-in predicates and arithmetic functions  
   - Logtalk standard libraries (see the libraries skill) instead of backend libraries  

5. **If non-portable code is unavoidable**  
   - Confine it to a **small number of entities**  
   - Expose a **portable interface** via a Logtalk **protocol** (and implementations per backend if needed)  
   - Use **conditional compilation** (`if/1`, `elif/1`, `endif/0`) with `current_logtalk_flag(prolog_dialect, Dialect)` (or related flags) only inside those isolated entities—not scattered through the app  

## Compiler support

Enable portability checking while developing portable code:

```logtalk
| ?- set_logtalk_flag(portability, warning).
```

(or pass `portability(warning)` in `logtalk_load/2`). Treat non-portable built-in / arithmetic warnings as issues to fix or encapsulate.

## Resource, numeric, and text portability

- Avoid `call_cleanup/2` and `setup_call_cleanup/3` in portable libraries and tests. Prefer catch-based deterministic resource helpers that close resources on success, failure, and exceptions.
- Do not shift right by 64 bits or more when packing bytes; backend behavior differs. Use division/modulo by 256 or smaller fixed-width words.
- Use standard character-code notation (`0'X`) or decimal numbers instead of `16'...` numeric notation.
- Use `number_codes/2` for numeric wire serialization; do not rely on a backend accepting numbers in `atom_codes/2`.
- For cross-backend fixtures containing curly Unicode quotes or dashes, construct the required representation from decimal character codes rather than embedding those characters in double-quoted literals; SICStus can report `layout_inside_quotes`.

## Patterns to prefer

| Goal | Prefer |
|------|--------|
| Lists, types, OS paths, random, … | Logtalk libraries + `uses/2` |
| Backend differences | Protocol + small implementing objects; conditional compilation only there |
| Settings per backend | Conditional compilation in `settings.lgt`, not in core logic |
| Strings / text in grammars | Representations independent of `double_quotes`; no `"..."` in DCG bodies for portable code |
| Flags | Do not rely on `unknown` or `double_quotes`; avoid other non-standardized flag assumptions |

## What to avoid

- SWI `library(...)`, SICStus libraries, or any backend module as a dependency of portable code
- Assuming `double_quotes` is `codes`, `chars`, or `atom`
- Assuming `unknown` is `error`, `warning`, or `fail`
- Double-quoted literals inside DCG rules in portable sources
- Scattering `:- if(current_logtalk_flag(prolog_dialect, ...))` through business logic instead of isolating it
- Ignoring `portability` linter warnings without encapsulation or replacement

## Verification checklist

- [ ] Compiles with `portability(warning)` cleanly, or only with documented encapsulated exceptions
- [ ] No proprietary Prolog libraries or built-ins in shared entities
- [ ] No dependency on `unknown` or `double_quotes` flag values
- [ ] No double-quoted terms in DCG rules
- [ ] Non-portable bits (if any) sit behind protocols in a minimal set of objects
- [ ] Core logic tested (or intended to run) on more than one backend when practical

## Further reading

- https://logtalk.org/handbook/userman/programming.html#portable-applications
- Libraries skill: prefer Logtalk libraries over backend ones
- Packs skill: declare portability metadata in pack `version/6` when publishing
