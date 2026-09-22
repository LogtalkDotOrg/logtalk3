---
name: performance
description: Write efficient Logtalk and Prolog predicates—first-argument indexing, avoid defaulty representations, no reverse/2 after head-to-tail accumulation, fix non-determinism without cuts on main APIs, prefer tail recursion. Use when optimizing code, reviewing hot paths, or fixing spurious choice-points.
---

# Performance Skill

Write predicates that index well, stay deterministic when they should, and avoid unnecessary work. Prefer clear structure that the backend can optimize over "clever" defaults and late cuts.

## Authoritative documentation

- Handbook performance discussion (linked from programming / performance sections of the Logtalk handbook): start from https://logtalk.org/handbook/userman/programming.html and the performance material it references
- Coding style and portability skills still apply; do not trade portability for micro-optimizations that depend on one backend

Do not assume multiple-argument indexing or backend-specific optimizations in portable code.

## When to use this skill

- Implementing recursive or multi-clause predicates on hot paths
- Tests or traces show unwanted non-determinism (`deterministic` tests failing, unexpected `redo`)
- Reviewing list construction, accumulators, or catchall clauses
- Refactoring after a performance or determinism regression

## Hard guidelines

### 1. Assume only first-argument indexing

Most Prolog systems index primarily (or only reliably) on the **first** argument.

- Whenever possible, put as the **first** argument an **input** that **discriminates** between clauses (a non-variable at call time whose principal functor/value selects one clause).
- Order clauses and design argument order so the discriminating term is first.
- Do not rely on multi-argument or deep indexing unless you have verified it on every target backend and accepted the portability cost.

### 2. Avoid defaulty representations

A **defaulty** representation encodes the common case as a bare term and the exceptional case with a wrapper (or the reverse), which forces a final catchall clause and hurts clarity and indexing.

- Prefer **non-defaulty** representations: every alternative has an explicit functor (e.g. `none` / `just(Value)`, `leaf` / `node(...)`, tagged unions).
- Structure data so clause heads can match explicit functors instead of “anything else."
- **Avoid catchall clauses** on main predicates when a better representation or argument order eliminates them.

### 3. Never accumulate with reverse/2 for head-to-tail lists

When recursively **constructing a list from head to tail**, do **not** push elements onto an accumulator and then call `reverse/2` at the end.

- Prefer difference lists, or direct head-tail construction in the output argument, or an algorithm that builds the result in the final order without a full reverse.
- Accumulators are fine when the natural order matches the accumulation order or when you use difference-list style open ends—not as an excuse for a mandatory `reverse/2` on the hot path.

### 4. Fix spurious choice-points at the source and not with cuts on main predicates

When tests expose **unwanted non-determinism**:

- **Do not** “fix" the public or main predicate by adding cuts as the first response.
- Find **exactly** what leaves the choice-point (extra clause match, unintended second solution, missing commit in a helper, soft-cut vs disjunction, etc.).
- Prefer: tighter heads, non-defaulty data, helper predicates that are deterministic by construction, explicit failure, or localized green cuts only where the logic is truly committed and documented.
- Use the debugger / `deterministic` outcomes in `lgtunit` (see testing and debugging skills) to locate the `redo`.

### 5. Prefer tail-recursive definitions

- Write **tail-recursive** predicates whenever possible (recursive call in tail position, constant stack under last-call optimization).
- Prefer accumulating parameters and a single recursive loop over deep non-tail recursion on large data.
- Combine with first-argument indexing so the recursive clause is selected efficiently.

## Quick patterns

| Situation | Prefer | Avoid |
|-----------|--------|--------|
| Multi-clause dispatch | Discriminating input as 1st arg | Key buried in 2nd/3rd arg only |
| Optional / union data | Explicit tags for every case | Bare term + catchall clause |
| Build list in order | Direct construction / difference lists | Acc + `reverse/2` |
| Extra choice-points | Fix representation or helpers | Cut on the main API predicate |
| Large recursion | Tail-recursive loop | Non-tail recursion blowing the stack |

## Verification checklist

- [ ] Clause selection can use first-argument indexing on a known input when it matters
- [ ] No defaulty representations that exists only to support a catchall whenever possible
- [ ] List construction does not rely on accumulator + `reverse/2` for head-to-tail results
- [ ] Determinism issues traced to cause; main predicates not papered over with cuts
- [ ] Recursive definitions are tail-recursive where practical
- [ ] Still portable (no backend-only indexing assumptions) unless explicitly scoped

## Further reading

- Logtalk handbook: programming and performance sections
- Testing skill: `deterministic` test outcomes
- Debugging skill: procedure box `redo` / choice-points
- Libraries skill: use efficient library predicates instead of reimplementing poorly
