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

# parstructs

This example requires using ECLiPSe as the backend Prolog compiler. It
illustrates using a ECLiPSe structure notation, which provides a solution
for using structures with field names rather than positional arguments. This
is accomplished by using a single parameter, instantiated to a structure, and
by defining a set of predicates for accessing the individual parameters by a
key (instead of using the Logtalk built-in `parameter/2` method that indexes
individual parameters by position). The access predicates are goal-expanded
to the corresponding ECLiPSe structure built-in predicates.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(parstructs(loader)).
```

Backtrackable update of an individual parameter:

```logtalk
obj(Dict)::init([a-1, b-2, c-3]), (obj(Dict)::b_set(b, 9), obj(Dict)::get(b, B), write(b-B), nl, fail; obj(Dict)::get(b, B), write(b-B), nl).
```

<!--
b-9
b-2
Dict = p{a:1, b:2, c:3}, B = 2.
-->

Non-backtrackable update of an individual parameter:

```logtalk
obj(Dict)::init([a-1, b-2, c-3]), (obj(Dict)::nb_set(b, 9), obj(Dict)::get(b, B), write(b-B), nl, fail; obj(Dict)::get(b, B), write(b-B), nl).
```

<!--
b-9
b-9
Dict = p{a:1, b:9, c:3}, B = 9.
-->
