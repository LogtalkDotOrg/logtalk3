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

This folder contains a set of simple examples illustrating how to use B-Prolog
constraint support with Logtalk. These examples are adapted with permission 
from the original author, Neng-Fa Zhou.

The B-Prolog `(::)/2` finite-domain built-in predicate clashes with the Logtalk 
`(::)/2` message-sending operator. The solution is to use instead the alternative
B-Prolog `in/2` built-in predicate.

The built-in `predicate_property/2` predicate fails to report some of the 
constraint predicates as built-in predicates. One workaround is to encapsulate
calls to these predicates using the `{}/1` Logtalk control construct.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(clp_bp(loader)).
```

```logtalk
clique::go.
```

<!--
clique(17)
clique(16)
clique(15)
clique(14)
clique(13)
clique(12)
clique(11)
clique(10)
clique(9)
clique(8)
clique(7)
clique(6)
clique(5)
{13,14,15,16,17}
cputime=447

true.
-->

```logtalk
magic::go.
```

<!--
[[2,6,38,41,42,43,3],[29,4,30,39,28,5,40],[22,32,24,33,31,17,16],[34,36,21,1,11,35,37],[19,25,44,15,47,13,12],[23,45,8,26,7,48,18],[46,27,10,20,9,14,49]]
execution time is 357milliseconds

true.
-->

```logtalk
puzzle::solve(V).
```

<!--
V = [9,5,6,7,1,0,8,2].
-->

```logtalk
steiner::go.
```

<!--
[{1,2,3},{1,4,5},{1,6,7},{1,8,9},{2,4,6},{2,5,8},{2,7,9},{3,4,9},{3,5,7},{3,6,8},{4,7,8},{5,6,9}]
cputime=80.
-->

```logtalk
queens3::top.
```

<!--
[1,3,5,56,53,4,61,7,52,70,55,81,6,82,50,57,8,63,58,77,83,49,9,40,65,72,47,64,59,10,54,51,75,67,41,43,18,11,28,89,62,74,48,69,33,17,12,66,73,34,38,42,68,91,31,19,13,96,93,86,90,79,71,95,60,76,20,14,78,2,84,94,87,36,30,88,35,45,21,15,92,85,80,27,22,46,44,39,37,29,24,26,16,23,25,32]

true.
-->

```logtalk
srq::q.
```

<!--
[[0,1,0,0,1,0,0,0,0,0],[0,0,1,1,0,1,0,1,0,0],[1,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,1],[0,0,0,0,0,0,1,0,1,0]]
time : 2

true.
-->

```logtalk
srq::q_all.
```

<!--
[[0,1,0,0,1,0,0,0,0,0],[0,0,1,1,0,1,0,1,0,0],[1,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,1],[0,0,0,0,0,0,1,0,1,0]]

no more solutions time all: 2

true.
-->
