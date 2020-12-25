________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This folder provides a classic set of plain Prolog benchmark programs and
Logtalk wrappers for those programs. Loading this example and an individual
plain Prolog benchmark allows us to evaluate the overhead of the implicit
execution context argument that the Logtalk compiler adds to object (and
category) predicates. Note that this old set of benchmarks don't cover all
features of modern day Prolog and Logtalk programming (e.g. meta-predicates).

All plain Prolog programs define a `top/0` predicate to run the benchmark.
The Logtalk versions provide a `top/0` public predicate.

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
