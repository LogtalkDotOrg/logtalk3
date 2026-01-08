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

# symbiosis

This folder contains several examples of using Prolog built-in meta-predicates
and module meta-predicates that take closures as arguments. It is used to help
test the Logtalk support for dealing with closures as it requires generation of
a helper predicate per call to workaround a clash between the way Logtalk
compiles predicates and the way a closure is extended to form a goal.

This example supports using ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog,
Trella Prolog, and YAP as the backend compilers.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(symbiosis(loader)).
```

Using the Prolog backend `maplist/2` predicate to check a list of integers:

```logtalk
symbiosis::p.
```

<!--
true.
-->

Using the Prolog backend `maplist/3` predicate to convert characters into codes:

```logtalk
symbiosis::q(L).
```

<!--
L = [97, 98, 99].
-->

Sorting lists of lists using the Prolog backend `maplist/3` predicate:

```logtalk
symbiosis::r(L).
```

<!--
L = [1, 2, 3].
-->

Adding one to each integer in a list using maplist/3 with a lambda expression with a built-in predicate:

```logtalk
symbiosis::s(L).
```

<!--
L = [2,3,4].
-->

Adding one to each integer in a list using maplist/3 with a lambda expression with a local predicate:

```logtalk
symbiosis::t(L).
```

<!--
L = [2,3,4].
-->
