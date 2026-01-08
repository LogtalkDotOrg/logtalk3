---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# bench

This folder provides a classic set of plain Prolog benchmark programs and
Logtalk wrappers for those programs. Loading this example and an individual
plain Prolog benchmark allows us to evaluate the overhead of the implicit
execution context argument that the Logtalk compiler adds to object (and
category) predicates. Note that this old set of benchmarks don't cover all
features of modern day Prolog and Logtalk programming (e.g., meta-predicates).

All plain Prolog programs define a `top/0` predicate to run the benchmark.
The Logtalk versions provide a `top/0` public predicate.

Some of the benchmarks have dependencies on some built-in predicates that
are not universal. But we load by default those benchmarks when their
dependencies don't actually prevent running the `top/0` predicate. See the
`loader.lgt` and `run.lgt` files for details.

Note that some benchmark programs cannot be loaded when using some backend
Prolog compilers due to portability issues. We decided to keep the original
code as-is instead of modifying them to fix those issues. See the `loader.lgt`
file for details.

The plain Prolog benchmark programs are often used and cited in scientific
papers that discusses Prolog performance, available in public repositories,
and public discussion forums. We copied them with permission from:

https://github.com/SWI-Prolog/bench

But this is just one of the public places are these programs can be found.
The copyright of the plain Prolog benchmark programs should be assumed to
belong to the original authors. Most of the programs lack any licensing
information or distribution terms, however. Some don't even mention the
author. Some go back to the earlier days of Prolog where programs were
often shared in an academic setting without much concern about licensing.
We redistribute these programs in good faith but understand that anyone
repackaging the Logtalk distribution may choose to delete this example
given the unclear legal status of some of these files.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(bench(loader)).
```

Run all tests, repeating each one 1000 times

```logtalk
run.
```

Run all tests, repeating each one N times, e.g. 2000 times

```logtalk
run(2000).
```

Run a specific benchmark, e.g. `nreverse`, 10000 times

```logtalk
run(nreverse, 10000).
```

If you want to compare Logtalk and plain Prolog versions of an individual
benchmark, load also its Prolog file (in this case, you must quit and
restart Logtalk for each testing scenario). For example:

```logtalk
['$LOGTALKUSER/examples/bench/boyer.pl'].
```

<!--
true.
-->

You can also use the `lgtunit` tool benchmark predicates directly. For
example:

```logtalk
lgtunit::benchmark(boyer::top,1000,Time).
```

<!--
Time = ...

true.
-->

For accurate timings of compiled `(::)/2` goals, the `lgtunit::benchmark/3`
calls should be made from compiled code in order to avoid the top-level
interpretation of the goals; an handy alternative is to use:

```logtalk
logtalk<<(lgtunit::benchmark(boyer::top,1000,Time)).
```

<!--
Time = ...

true.
-->

Some Prolog compilers such as SWI-Prolog, Trealla Prolog, XVM, and YAP
provide a handy `time/1` predicate that may also be used in alternative
to the `lgtunit` benchmark predicates. The adapter files for SWI-Prolog
and YAP ensure that `(::)/2` goals in the argument of the `time/1`
meta-predicate are fully compiled prior to calling them so that we
benchmark the code instead of the Logtalk compiler.

Confirm that the `time/1` predicate is available:

```logtalk
time(true).  % autoload if necessary
```

Benchmark some queries:

```logtalk
time((between(1,1000,_),top,fail;true)).
```

```logtalk
time((between(1,1000,_),boyer::top,fail;true)).
```
