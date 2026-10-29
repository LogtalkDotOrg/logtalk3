---
name: logtalk-documenting
description: Document Logtalk entities and predicates using info/1, info/2, mode/2, and related directives, and generate API docs with lgtdoc. Use when adding or improving source documentation, writing info directives, or producing HTML/PDF API documentation.
license: Apache-2.0
metadata:
  copyright: "2026 Paulo Moura <pmoura@logtalk.org>"
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
4. Follow the handbook style for comments (third-person singular, "True iff …", "Enumerates, by backtracking, …", "Fails if …").
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

- Start with a verb in third-person singular ("Runs …", "Converts …") or with "True iff …" / "True if …".
- Multi-solution predicates: "Enumerates, by backtracking, all/the …".
- Failure conditions: "Fails if …" / "Fails when …".
- Keep the main `comment` concise; move detail to `remarks` or `fails_if`.
- Focus `comment` values on the contract rather than repeating library notes; wrap inline code in double backquotes.

### Exceptions

List exceptions with the `exceptions` key. Prefer standard error terms. Typical order: instantiation → type → domain → others. Use the same argument names as in `arguments`/`argnames`. Follow the controlled phrasing in the handbook section on documentating exceptions.

Every predicate that can throw exceptions must document them using `exceptions is [...]`, including exceptions propagated from validation or delegated calls.

### Declarations and inherited metadata

- Genuinely new locally declared predicates need explicit `mode/2` directives.
- Do not redeclare inherited protocol predicates just to repeat `public/1`, `mode/2`, or `info/2`; inherit the shared declarations and metadata.
- A local redeclaration of an inherited predicate hides its inherited definition. When a definition specialization is necessary, call the inherited definition using `^^/1`.
- A documented local validation helper only used in tests needs an explicit scope (normally private) to avoid dead code lint warnings. Use protected scope for helpers needed by specializations.
- Document each protected predicate declaration immediately with `mode/2` and `info/2` directives. In the `info/2` directive, use concise `comment` and `argnames`.
- New library unit-test objects need `info/1` directive with version, author, date, and a library-specific unit-test comment.

## Library notes and distribution files

- Describe predicates in complete sentences: "The `print_recommender/1` predicate prints ...". Use "predicates" for grouped descriptions rather than terse predicate-label fragments.
- Introduce displayed model or data terms with a sentence identifying what they represent and which predicates return them; a section heading alone is insufficient.
- In the Logtalk distribution, edit source documentation only. Do not regenerate or edit generated Handbook pages unless explicitly requested.
- New distribution source and documentation files must retain the complete standard Apache 2.0 header, including the license text; do not replace it with abbreviated SPDX-only lines.

## Generating documentation

1. Compile the code with `source_data(on)` (often already set for libraries).
2. Load and run `lgtdoc` to produce per-entity XML files (and optional indexes) only when explicitly asked.
3. Convert XML to the desired final format (HTML, PDF, reStructuredText, Markdown, ...) using the scripts and stylesheets under `lgtdoc/xml` (see the `lgtdoc` tool documentation).

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
- [ ] When generation is requested, `lgtdoc` successfully generates XML (and requested downstream formats)
- [ ] Cross-references (`see_also`) point to real entities/predicates

## Further reading

- Documenting user manual: https://logtalk.org/handbook/userman/documenting.html
- `lgtdoc` tool: https://logtalk.org/handbook/devtools/lgtdoc.html
- Reflection: used by `lgtdoc` to extract documenting data
- Examples: library and tool APIs in the Logtalk distribution
