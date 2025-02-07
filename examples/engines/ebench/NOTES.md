---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
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

# engines - ebench

This example provides benchmarking support for evaluating the performance
of threaded engine creation and destroying. Currently it runs on ECLiPSe,
SWI-Prolog, and XVM. It should run also on YAP if and when this system
threads bugs are fixed.

Confirm that the `time/1` predicate is available:

```logtalk
time(true).
```

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(ebench(loader)).
```

First, an example with an engine goal that succeeds deterministically:

```logtalk
time((between(1,2000,I),atom_number(A,I),threaded_engine_create(_,true,A),fail)).
```

```logtalk
time((between(1,2000,I),atom_number(A,I),threaded_engine_destroy(A),fail)).
```

Second, an example with an engine goal that provides an infinite stream of solutions:

```logtalk
time((between(1,2000,I),atom_number(A,I),threaded_engine_create(_,repeat,A),fail)).
```

```logtalk
time((between(1,2000,I),atom_number(A,I),threaded_engine_destroy(A),fail)).
```

Finally, an example with an engine running a loop predicate using the engine term queue:

```logtalk
assertz((loop :- threaded_engine_fetch(_),loop)).
```

```logtalk
time((between(1,2000,I),atom_number(A,I),threaded_engine_create(_,loop,A),fail)).
```

```logtalk
time((between(1,2000,I),atom_number(A,I),threaded_engine_destroy(A),fail)).
```
