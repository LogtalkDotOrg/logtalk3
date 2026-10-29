---
name: logtalk-testing
description: Write, organize, run, and debug Logtalk unit tests with lgtunit. Use when adding tests, fixing failing tests, improving coverage, setting up tester drivers, or using QuickCheck.
license: Apache-2.0
metadata:
  copyright: "2026 Paulo Moura <pmoura@logtalk.org>"
---

# Testing Skill

Use the official `lgtunit` tool for all unit testing. Do not invent a custom test framework.

## Authoritative documentation

Read and follow:

- https://logtalk.org/handbook/devtools/lgtunit.html

Do not duplicate the handbook content. Prefer linking to specific sections when more detail is needed.

## When to use this skill

- Adding or extending tests for objects, categories, protocols, or plain Prolog code
- Creating or updating a `tester.lgt` (or `tester.logtalk`) driver
- Diagnosing failing, flaky, or non-deterministic tests
- Adding code coverage or QuickCheck property tests
- Automating tests with `logtalk_tester`

## Core workflow

1. **Ensure `lgtunit` is available**
   ```logtalk
   | ?- logtalk_load(lgtunit(loader)).
   ```

2. **Define tests in objects that extend `lgtunit`**
   - Prefer one primary test object named `tests` for simple cases.
   - For new library test objects, include `info/1` metadata with version, author, date, and a library-specific unit-test comment.
   - Use descriptive, unique, ground test identifiers (usually atoms).
   - Keep test objects focused; put supporting code in separate entities/files (e.g., `test_objects.lgt`).
   - Add `cover/1` facts before the tests for each tested object and category to collect code coverage stats.

3. **Compile tests with the required hook**
   ```logtalk
   | ?- logtalk_load(tests, [hook(lgtunit)]).
   ```
   Add `optimize(on)` for debugged deterministic tests. Use `debug(on)` while investigating failures.

4. **Provide a driver file (`tester.lgt`)**
   - Load libraries, the code under test, `lgtunit`, and the tests.
   - Call `tests::run` (or `lgtunit::run_test_sets/1` for multiple sets).
   - See the sample driver in the handbook and the `samples/tester-sample.lgt` file.

5. **Run tests**
   - Interactive: `tests::run` or `tests::run(TestId)`.
   - Automated: `logtalk_tester -p <backend>` (looks for `tester.lgt` / `tester.logtalk` by default).

## Preferred test dialects (in order of usefulness)

- `test(Test, Outcome, Options) :- Goal.` — with `condition/1`, `setup/1`, `cleanup/1`, `flaky`, `note/1`
- `test(Test, Outcome) :- Goal.` — full control (`true`, `deterministic`, `fail`, `error(Error)`, `ball(Ball)`, assertions, `subsumes/2`, `variant/2`, `all/1`, `exists/1`, …)
- `test(Test) :- Goal.` — simplest success test
- `quick_check(Test, Template)` / `quick_check(Test, Template, Options)` — property-based testing

Always give tests clear, intention-revealing identifiers. Duplicate or non-ground identifiers are errors.

## Assertions and fixtures

- Use `lgtunit::variant/2` for variant comparisons, not backend-specific `=@=/2` or `term::variant/2`.
- For floating-point comparisons (including lists of floats), import `op(700, xfx, =~=), (=~=)/2` from `lgtunit` and use `=~=/2`. Avoid `=:=/2` or `==/2` against float literals, which trigger suspicious-call lint warnings. Test tolerance does not imply tolerance in library arithmetic.
- Write property tests using `quick_check(Test, Template, Options)` or `quick_check(Test, Template)` rather than calling `^^quick_check` in test bodies.
- Keep one goal per line in tests and fixtures. Reuse existing test objects and helpers instead of adding unnecessary files or custom assertion infrastructure.
- Resource helpers and text fixtures must follow the portability skill; do not use backend-only cleanup predicates in portable tests.

## Parametric and multi-set tests

- Use parametric test objects (e.g. `tests(_Implementation_)`) to run the same suite against multiple implementations of a protocol.
- Run several test sets together with `lgtunit::run_test_sets/1` when you need a combined coverage report.

## Code coverage and debugging

- Compile the code under test with `source_data(on)` and also `debug(on)` when coverage is required.
- The handbook and `tools/lgtunit/NOTES.md` describe coverage reports and debugging techniques for failing tests.
- Re-run individual tests with `tests::run(TestId)` while debugging.

## Automation and CI

- Use the `logtalk_tester` script for local and CI runs.
- Respect its exit status conventions (failed tests, broken sets, timeouts, crashes).
- Prefer a dedicated fast driver (no coverage, optimized compilation) and a coverage driver when both are useful.

### Test execution policy

- When the task defines an explicit test gate, run it after each step without asking again. Otherwise, ask before running tests after making changes.
- Never run the repository-wide test suite. Run the tester from the relevant library's own directory; do not rely on a directory-selection flag from the repository root.
- Use `logtalk_tester -p <backend> -t <seconds>` to detect hangs. If the installed shim is broken, use the checkout's `scripts/logtalk_tester.sh`.
- For checkout runs, first change to the repository root and export `LOGTALKUSER` as its absolute path in the same terminal command, then change to the library directory. Also export `LOGTALKHOME` as the checkout's absolute path when testing library aliases or `paths/paths.pl`.
- Do not set `LOGTALKUSER=$PWD` after earlier directory changes; terminals preserve their working directory and may point at a nested library.
- Leave generated `.lgt_tmp` and `logtalk_tester_logs` directories in place; never delete them after testing.

## What to avoid

- Putting non-test entities in the same source file as the test object (term-expansion applies to the whole file); use instead a separate file, e.g, `test_entities.lgt`.
- Relying on test order or shared mutable state without explicit `setup/1` / `cleanup/1`.
- Using overly generic test names (`test1`, `check`, …).
- Ignoring non-determinism when the predicate under test should be deterministic.
- Duplicating handbook prose or inventing parallel testing conventions.

## Verification checklist

- [ ] Tests compile cleanly with `hook(lgtunit)`
- [ ] All test identifiers are unique and descriptive
- [ ] `tests::run` (or `logtalk_tester`) reports the expected results
- [ ] Failing tests have been minimized and debugged
- [ ] Driver file loads dependencies in a sensible order (libraries → code under test → tests)
- [ ] Collecting code coverage is the default except for performance tests

## Further reading

- Full tool documentation: https://logtalk.org/handbook/devtools/lgtunit.html
- Additional testing advice: https://logtalk.org/testing.html
- Sample files: `samples/tests-sample.lgt`, `samples/tester-sample.lgt`
- Examples and library tests under the Logtalk distribution `tests/`, `library/`, and `examples/` directories
