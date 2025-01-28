---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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

# engines - emetas

This example requires support for both threads and coroutining. Currently it
runs on ECLiPSe and SWI-Prolog. It should run also on XSB and YAP if and when
these systems bugs with coroutining and/or threads get fixed.

This folder contains examples of using threaded engines for implementing
meta-predicates. The `best_of/3` meta-predicate example, is described in
the paper:

@article{Tarau2000,
	author="Paul Tarau",
	title="Architecture and Implementation Aspects of the Lean Prolog System",
	url="http://www.cse.unt.edu/~tarau/research/LeanProlog/ArchitectureOfLeanProlog.pdf"
}

Load the example:

```logtalk
logtalk_load(emetas(loader)).
```

<!--
true.
-->

Some clauses for a predicate used in the next queries:

```logtalk
assertz(a(2)), assertz(a(1)), assertz(a(4)), assertz(a(3)).
```

<!--
true.
-->

Find the larger value from the values returned by a generator:

```logtalk
emetas::best_of(X, (>), a(X)).
```

<!--
X = 4.
-->

Test the threaded engine implementation of the standard `findall/3` meta-predicate:

```logtalk
emetas::find_all(X, a(X), List).
```

<!--
List = [2, 1, 4, 3].
-->

Test the threaded engine implementation of the `find_at_most/4` meta-predicate:

```logtalk
emetas::find_at_most(3, X, a(X), Xs).
```

<!--
Xs = [2, 1, 4].
-->

```logtalk
emetas::find_at_most(7, X, a(X), Xs).
```

<!--
Xs = [2, 1, 4, 3].
-->
