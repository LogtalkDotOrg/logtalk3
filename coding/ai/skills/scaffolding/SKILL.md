---
name: scaffolding
description: Scaffold a new Logtalk application (directory, settings.lgt, loader.lgt, optional tester/tests). Use when starting a project from scratch or setting up loaders, settings, and unit-test drivers.
---

# New Application Skill

Create a clean Logtalk application layout outside the Logtalk user folder, with settings, loader, and optional tests. Prefer the official sample files over inventing a parallel structure.

## Authoritative resources

- Sample files in the Logtalk distribution: `samples/settings-sample.lgt`, `samples/loader-sample.lgt`, `samples/tester-sample.lgt`, `samples/tests-sample.lgt`
- Customization notes: `CUSTOMIZE.md` in the Logtalk distribution (and comments inside the sample files)
- Coding style (project layout): https://logtalk.org/coding_style_guidelines.html
- Related skills: `testing`, `documenting`, `coding-style`, `libraries`, `portability`, `packs`

Do not put application sources under the Logtalk user directory; that tree is refreshed when Logtalk is updated.

## When to use this skill

- Starting a new Logtalk library or application
- Adding a proper `loader.lgt` / `tester.lgt` / `settings.lgt` to an existing ad-hoc project
- Onboarding an AI agent to scaffold the standard file set

## Main steps

Use the `LOGTALKUSER` environment variable to locate the sample files in the user local Logtalk installation.

### 1. Create an application directory

- Choose a **simple, descriptive** directory name (avoid names that clash with Logtalk distribution libraries or tools).
- Prefer a location **outside** your Logtalk user folder (the folder updated on Logtalk upgrades).
- All application sources, loaders, tests, and notes live under this directory (or clear subdirectories you define).

### 2. Optional settings file (`settings.lgt`)

- Copy or rename `samples/settings-sample.lgt` from the Logtalk distribution to **`settings.lgt`**.
- Typical location: Logtalk user directory or another place Logtalk loads at startup (see sample comments and `CUSTOMIZE.md`); relying on a settings file only inside the application directory requires always starting Logtalk from that directory.
- Edit it to, as needed:
  - Preload developer tools (e.g. `help`, `debugger`)
  - Define **library aliases** for your application(s) via `logtalk_library_path/2`
  - Set **default compiler flags**
  - Other customizations documented in the sample and `CUSTOMIZE.md`
- Keep settings portable where possible; use conditional compilation for backend-specific options (see the portability skill).

### 3. Loader and tests in the application directory

**Loader**

- Copy `samples/loader-sample.lgt` into the application directory as **`loader.lgt`**.
- Edit it to load libraries your app needs, then your application source files, in a sensible order (dependencies before dependents).
- Prefer loading third-party/Logtalk libraries here so entities exist before app files compile (helps static binding and fewer warnings).

**Tests (recommended)**

- Copy `samples/tester-sample.lgt` → **`tester.lgt`** and `samples/tests-sample.lgt` → **`tests.lgt`** (or a `tests/` directory later if the suite grows).
- Edit `tests.lgt` so the test object extends `lgtunit` and defines your tests.
- Edit `tester.lgt` to load `lgtunit`, the code under test (often via `loader` or explicit loads), the tests with `hook(lgtunit)`, and run them (see the testing skill and handbook).
- Name `tester.lgt` that way so `logtalk_tester` finds it by default.
- For large applications, create if necessary a `test_files` directory with the `tests.lgt` file and additional testing resources. But keep the `tester.lgt` file in the main directory, side-by-side with the application main `loader.lgt` file.

### 4. Add source files

- Create entities in `.lgt` / `.logtalk` files following the coding-style and documenting skills.
- Register new files in `loader.lgt` (and ensure tests cover them via `tester.lgt`).
- Prefer Logtalk standard libraries over reimplementing utilities (libraries skill).

## Minimal tree (typical)

```text
my_app/
    loader.lgt
    tester.lgt          % optional but recommended
    tests.lgt           % optional but recommended
    ... application sources ...
    NOTES.md            % optional
```

Plus a configured `settings.lgt` in the location Logtalk uses for startup settings (often the Logtalk user directory), defining a library alias that points at `my_app` if you load via library notation.

## After scaffolding

```logtalk
| ?- logtalk_load(loader).     % from the app directory or via library alias
| ?- logtalk_load(tester).     % compile/run tests
```

Automated tests from the app directory:

```text
logtalk_tester -p <backend>
```

## What to avoid

- Creating the project **inside** the Logtalk user folder (risk of loss or clutter on upgrade)
- Hand-rolled loader/tester structure that ignores the sample files and `lgtunit` conventions
- Forgetting to add new sources to `loader.lgt`
- Loading app files before their library dependencies
- Skipping `hook(lgtunit)` when compiling test objects

## Verification checklist

- [ ] Application directory is outside the Logtalk user folder
- [ ] `settings.lgt` is based on `settings-sample.lgt` and sets needed tools, aliases, and flags
- [ ] `loader.lgt` loads dependencies then application sources successfully
- [ ] If tests exist: `tester.lgt` / `tests.lgt` run cleanly with `lgtunit`
- [ ] Library alias (if any) resolves and matches the directory name you chose
- [ ] Layout matches coding-style guidelines for loaders, testers, and file names

## Further reading

- Distribution: `samples/*-sample.lgt`, `CUSTOMIZE.md`
- https://logtalk.org/coding_style_guidelines.html
- Testing skill + https://logtalk.org/handbook/devtools/lgtunit.html
- Portability skill for multi-backend settings and code
