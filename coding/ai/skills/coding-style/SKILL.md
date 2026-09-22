---
name: coding-style
description: Apply and review Logtalk coding style guidelines (layout, naming, directives, clauses). Use when writing or reformatting Logtalk code, reviewing style, or matching distribution conventions.
---

# Coding Style Skill

Follow the official Logtalk coding style guidelines. Treat them as strong recommendations; apply them strictly when contributing to the Logtalk distribution, and aim for consistency within any application or library.

## Authoritative documentation

Read and follow:

- https://logtalk.org/coding_style_guidelines.html

Do not duplicate that page. Prefer linking to it. General Prolog style guidelines also apply unless overridden here or in the official document.

## When to use this skill

- Writing new Logtalk source files or entities
- Reformatting or cleaning up existing code
- Reviewing a change or PR for style
- Aligning application or library code with Logtalk distribution conventions
- Setting up loaders, testers, doclets, and project directory layout

## Always-on rules (also suitable for AGENTS.md)

These should hold for almost every edit:

- **Indentation**: tabs only for indentation; never mix tabs and spaces. Use spaces only for non-indent alignment. One tab = one level. Code must stay correctly indented when tab width changes.
- **Predicate names**: `snake_case`, descriptive, avoid abbreviations, avoid CamelCase and mid-name digits. Prefer reuse of established API names (`member/2`, `size/2`, …).
- **Variable names**: `CamelCase`, descriptive, avoid abbreviations. Singletons: `_` or `_Name`.
- **Dynamic predicates**: trailing underscore convention (e.g. `state_`) is common in Logtalk.
- **Goals**: one goal per line in clause bodies (small exceptions: green cuts, `nl` after write). Red cuts on their own line.
- **Disjunctions**: always parenthesized; `;` at the start of the line; prefer separate clauses when the disjunction is the whole body (with noted exceptions).
- **If-then-else**: always parenthesized; always include the else branch; consistent vertical alignment of `(`, `->` / `;`, `)`.
- **Operators and lists**: spaces around binary operators; space after commas and after `|` in lists.
- **Directives vs rules**: space after `:-` in directives; space before `:-` in clause rules.
- **Documentation**: prefer `info/1`, `info/2`, and `mode/2` over long unstructured comments; put non-obvious rationale in comments on their own line.

## Project and file organization

- Directory with a simple, descriptive name (avoid clashing with Logtalk distribution names).
- Typical files: `loader.lgt`, `tester.lgt`, `tests.lgt` (or a `tests/` sub-directory), optional `doclet.lgt`, `NOTES.md`, `SCRIPT.txt`.
- Single-entity file: name the file after the entity; parametric entities append the parameter count (e.g. `sort_1.lgt`).
- Multi-entity file: use a descriptive name, often matching the directory.
- Default extensions: `.lgt` or `.logtalk`.
- Prefer US-ASCII or UTF-8.

## Layout patterns to prefer

- `initialization/1` and multi-line directives: indent arguments one tab; align closing `)` with the directive so tab width does not break alignment.
- Long clause heads: break after the opening `(`, one logical group of arguments per line; do not try to align under the predicate name with spaces.
- Complex goals (`setof/3`, etc.): multi-line with clear parenthesis alignment.
- Failure-driven loops: indent the body goals between generator and `fail`; consider `forall/2` when unexpected failure must not be masked.
- Meta-arguments: keep them simple (ideally a single call); use lambdas when they avoid a throwaway auxiliary.

## Comments

- Entity/predicate purpose → `info/1` and `info/2` (see the documenting skill).
- Inline comments on their own line, aligned with the code they describe—not trailing on the same line as a goal.
- Comment only what is not obvious from predicate and variable names.

## Sequences and accumulators

Represent successive states as `S0`, `S1`, …, `S` (with a meaningful base name), e.g. `Sum0`, `Sum1`, `Sum`. Avoid generic names such as `Acc`.

## What not to put in source files

Top-level interpreter shortcuts for loading or `make` are not part of the language. Use them only at the top level, never in source.

## What to avoid

- Mixing tabs and spaces for indentation
- CamelCase predicate names or snake_case variable names
- Unparenthesized disjunctions or if-then-else; omitting the else branch
- Dense multi-goal lines and complex inline meta-arguments
- Long trailing comments on the same line as code
- Duplicating the full guidelines text instead of linking to them
- Inconsistency within a file or library (pick a valid if-then-else layout and stick to it)

## Verification checklist

- [ ] Indentation is tabs-only and stable under different tab widths
- [ ] Predicate and variable naming follow the conventions above
- [ ] Disjunctions and if-then-else are parenthesized and formatted consistently
- [ ] Loaders/testers/doclets and file names match the usual project layout
- [ ] Declared predicates have appropriate `mode/2` / `info/2` where documentation is expected
- [ ] Declared predicates that can throw error must have all possible exceptions documented in their `info/2` directives
- [ ] No top-level shortcuts embedded in source files
- [ ] Style is consistent with neighboring code in the same library or application

## Further reading

- Coding style guidelines: https://logtalk.org/coding_style_guidelines.html
- Documenting: https://logtalk.org/handbook/userman/documenting.html (and the `documenting` skill)
- Declaring predicates / `mode/2`: Handbook section on predicates
- Sample layout and ignore files: Logtalk distribution `coding/` directory
