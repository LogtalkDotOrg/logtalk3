---
name: documenting
description: Document Logtalk entities and predicates using info/1, info/2, mode/2, and related directives, and generate API docs with lgtdoc. Use when adding or improving source documentation, writing info directives, or producing HTML/PDF API documentation.
---

# Documenting Skill

Document Logtalk source code with the official directives and the `lgtdoc` tool. Do not invent a parallel documentation scheme.

## Authoritative documentation

Read and follow:

- https://logtalk.org/handbook/userman/documenting.html

Do not duplicate the handbook content. Prefer linking to specific sections when more detail is needed. Related tools: https://logtalk.org/handbook/devtools/lgtdoc.html

## When to use this skill

- Adding or updating entity-level documentation (`info/1`)
- Documenting predicates or non-terminals (`info/2`, `mode/2`, `meta_predicate/1`, …)
- Describing arguments, exceptions, examples, and failure conditions
- Generating XML / HTML / PDF API documentation with `lgtdoc`
- Improving consistency of library or application API docs

## Core principles

1. Turn `source_data` on when you want the compiler to retain documenting information for `lgtdoc`.
2. Use the standard directives; they are visible to reflection and to `lgtdoc`.
3. Prefer short, precise comments. Put longer explanations under `remarks` or `fails_if`.
4. Follow the handbook style for comments (third-person singular, “True iff …", “Enumerates, by backtracking, …", “Fails if …").
5. Use controlled language for exceptions: https://logtalk.org/handbook/userman/documenting.html#documenting-predicate-exceptions

## Entity documentation (`info/1`)

Place an `info/1` directive in the entity. Predefined keys include:

- `comment` — purpose (atom, end with a period)
- `author`, `version` (`Major:Minor:Patch`), `date` (`Year-Month-Day`)
- `parameters` / `parnames` — for parametric entities
- `copyright`, `license` (prefer SPDX identifiers)
- `remarks` — list of `Topic-Text` pairs
- `see_also` — related entities

Use only the keys that make sense; custom keys are allowed and will appear in generated docs.

## Predicate / non-terminal documentation (`info/2`)

```logtalk
:- info(Name/Arity, [
    comment is '...',
    arguments is ['Name'-'Description.', ...],   % or argnames
    fails_if is '...',
    exceptions is ['Description' - ExceptionTerm, ...],
    examples is ['Description' - Goal - {Bindings}, ...],
    remarks is ['Topic'-'Text.', ...],
    since is Major:Minor:Patch,
    see_also is [Other/Arity, ...]
]).
```

Also document modes with `mode/2` and meta-predicates with `meta_predicate/1` where applicable. Non-terminals use `Name//Arity`.

### Style guidelines for comments

- Start with a verb in third-person singular (“Runs …", “Converts …") or with “True iff …" / “True if …".
- Multi-solution predicates: “Enumerates, by backtracking, all/the …".
- Failure conditions: “Fails if …" / “Fails when …".
- Keep the main `comment` concise; move detail to `remarks` or `fails_if`.

### Exceptions

List exceptions with the `exceptions` key. Prefer standard error terms. Typical order: instantiation → type → domain → others. Use the same argument names as in `arguments`/`argnames`. Follow the controlled phrasing in the handbook.

## Generating documentation

1. Compile the code with `source_data(on)` (often already set for libraries).
2. Load and run `lgtdoc` to produce per-entity XML files (and optional indexes).
3. Convert XML to the desired final format (HTML, PDF, reStructuredText, Markdown, …) using the scripts and stylesheets under `lgtdoc/xml` (see the `lgtdoc` tool documentation).

## What to avoid

- Long, unstructured comments instead of structured `info` keys
- Omitting `mode/2` for public predicates
- Inconsistent argument names between `info/2`, `mode/2`, and exception descriptions
- Duplicating handbook text or inventing non-standard documentation mechanisms
- Forgetting that documenting directives are data: keep values bound and well-formed

## Verification checklist

- [ ] Entities that should appear in the API docs have a useful `info/1`
- [ ] Public (and other documented) predicates have `info/2` and appropriate `mode/2`
- [ ] Comments follow the handbook style and end with a period where required
- [ ] Exceptions use standard terms and consistent argument names
- [ ] `lgtdoc` successfully generates XML (and downstream formats) for the library or application
- [ ] Cross-references (`see_also`) point to real entities/predicates

## Further reading

- Documenting user manual: https://logtalk.org/handbook/userman/documenting.html
- `lgtdoc` tool: https://logtalk.org/handbook/devtools/lgtdoc.html
- Reflection: used by `lgtdoc` to extract documenting data
- Examples: library and tool APIs in the Logtalk distribution
