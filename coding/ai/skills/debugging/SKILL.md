---
name: debugging
description: Debug Logtalk code with the debugger tool, debug mode, breakpoints, tracing, and related techniques. Use when diagnosing failures, unexpected solutions, exceptions, or non-determinism in Logtalk entities.
---

# Debugging Skill

Use Logtalk’s official debugger and debugging API. Entities must be compiled in **debug mode** to be traceable. Do not invent a parallel debugging workflow.

## Authoritative documentation

Read and follow:

- https://logtalk.org/handbook/userman/debugging.html

Tool reference: https://logtalk.org/handbook/devtools/debugger.html

Do not duplicate those pages. Prefer linking to specific sections when more detail is needed.

## When to use this skill

- A goal fails, succeeds incorrectly, or is unexpectedly non-deterministic
- An exception needs to be traced to its source
- Setting or refining breakpoints (predicate, clause, conditional, hit-count, triggered, context)
- Understanding control flow via the procedure box model (call, exit, redo, fail, exception, fact, rule)
- Switching between debug, normal, and optimized compilation while diagnosing an issue

## Core workflow

1. **Load the debugger**
   ```logtalk
   | ?- logtalk_load(debugger(loader)).
   ```
   Optionally load it from a settings file at startup.

2. **Compile the code under investigation in debug mode**
   - Globally: `set_logtalk_flag(debug, on)` (also turns off `optimize`).
   - Per load: `logtalk_load(Files, [debug(on)])`.
   - Recompile loaded files: `logtalk_make(debug)` (or `{+d}` with many backends).
   - Keep the `clean` flag on when turning `debug` on at runtime so previously compiled files are regenerated.
   - Per entity/file: `set_logtalk_flag(debug, on)` directive inside the entity or at the start of the file.

3. **Confirm what is debuggable**
   ```logtalk
   | ?- debugger::debugging(Entity).
   ```

4. **Activate tracing or debugging**
   - `debugger::trace` / `debugger::debug` — select the debugger as the active debug handler.
   - `debugger::nodebug` — deselect it (only one debug handler is active at a time; e.g. `ports_profiler` would be deactivated).

5. **Set breakpoints as needed**, run the failing scenario, interpret ports, then remove or tighten breakpoints.

6. **After fixing**, recompile in normal or optimized mode (`logtalk_make(normal)`, `logtalk_make(optimal)`, or `{+n}` / `{+o}`) for performance.

## Procedure box model (summary)

Ports: `call`, `exit`, `redo`, `fail`, `exception`, plus Logtalk’s `fact` and `rule` (head unification with a fact or rule).

Control which ports pause for interaction:

```logtalk
| ?- debugger::leash([call, exit, fail]).
| ?- debugger::leash(loose).   % or half, tight, full, none
```

Default is to pause at every port. Unleashed ports are printed but do not stop.

## Breakpoint types (use the simplest that works)

| Type | Typical API | Pauses when |
|------|-------------|-------------|
| Predicate / non-terminal | `spy(Name/Arity)`, `spy(Name//Arity)`, optionally `Object::Name/Arity` | All ports for that predicate |
| Clause | `spy(Entity-Line)` | Unification at that clause head |
| Conditional | `spy(Entity, Line, Condition)` | Unification and condition holds (lambda or hit-count expression) |
| Hit count | Condition is e.g. `=<(N)`, `=:=(N)`, `mod(M)`, `>(N)` | Unification count matches expression |
| Triggered | Condition is another `Entity-Line` breakpoint | That other breakpoint was hit earlier |
| Context | `spy(Sender, This, Self, Goal)` | Current execution context and goal are subsumed by the templates |

Remove with the matching `nospy/...` predicates (including `nospy(_)` to clear matching predicate/clause breakpoints). Setting a clause breakpoint clears conflicting conditional/triggered/log points on the same clause.

Conditional lambdas run in `user`, must be side-effect free, and may use forms like `[Goal]>>Cond` or `[Count, N, Goal]>>Cond`.

## Practical tips

- Prefer **predicate** or **clause** breakpoints first; add conditions or context only when the trace is too noisy.
- Line numbers for clause breakpoints depend on the backend’s accuracy; see the debugger tool notes if locations look wrong.
- For test failures, combine with the `testing` skill: compile with `debug(on)`, re-run a single test (`tests::run(TestId)`), and spy the relevant predicates.
- Determinism: watch for unexpected `redo` ports or missing determinism when `deterministic/1` tests fail.
- Exceptions: leash `exception` (e.g. `tight` or `full`) and spy the predicate that should throw or catch.
- Log points and advanced features are described in the handbook; use them when you need trace output without stopping.

## What to avoid

- Expecting to trace entities compiled in normal or optimized mode
- Leaving `debug(on)` and heavy leashing on for routine work (slow, noisy)
- Side effects inside conditional breakpoint lambdas
- Confusing Logtalk’s debugger with backend Prolog debuggers (different feature set; Logtalk debugger is a normal application using the debugging API)
- Ignoring the single active debug handler rule when `ports_profiler` or other handlers are loaded

## Verification checklist

- [ ] Relevant entities show up under `debugger::debugging/1`
- [ ] Debugger is the active handler (`trace`/`debug` as appropriate)
- [ ] Breakpoints are minimal and removed or disabled when done
- [ ] Failing scenario is reproducible under the debugger
- [ ] After the fix, code is recompiled without debug if performance matters
- [ ] Tests (if any) pass for the previously failing case

## Further reading

- Debugging user manual: https://logtalk.org/handbook/userman/debugging.html
- Debugger tool: https://logtalk.org/handbook/devtools/debugger.html
- Compiler flags: `debug`, `optimize`, `clean`
- Related: `ports_profiler`, settings files for loading the debugger at startup
