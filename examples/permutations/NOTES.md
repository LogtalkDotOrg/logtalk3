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

# permutations

This folder provides a Logtalk port of Prolog benchmarks based on list
permutations. Some of the predicate definitions are purposively not
tail-recursive. A non-optimized map list predicate is also used. The
original code was written and contributed by Paul Tarau.

Load the example:

```logtalk
logtalk_load(permutations(loader)).
...

Some example benchmark queries for backend Prolog compilers implementing
the `time/1` timing predicate (e.g., SWI-Prolog, Trealla Prolog, YAP, XVM, 
and others):

```logtalk
time(true).  % auto-load the predicate in the case of SWI-Prolog
```

<!--
true.
-->

```logtalk
time(permutations::backtracking(7)).
```

<!--
% 5,945 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 11388889 Lips)
true.
-->

```logtalk
time(permutations::list(7, _)).
```

<!--
% 24,029 inferences, 0.002 CPU in 0.003 seconds (78% CPU, 11150348 Lips)
true.
-->

```logtalk
time(permutations::all(7, _)).
```

<!--
% 10,993 inferences, 0.002 CPU in 0.003 seconds (79% CPU, 5091709 Lips)
true.
-->

```logtalk
time(permutations::map(7, _)).
```

<!--
% 44,190 inferences, 0.006 CPU in 0.007 seconds (80% CPU, 7770353 Lips)
true.
-->

```logtalk
time(permutations::copy(7)).
```

<!--
% 34,110 inferences, 0.005 CPU in 0.006 seconds (79% CPU, 7558165 Lips)
true.
-->
