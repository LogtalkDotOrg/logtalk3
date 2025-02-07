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

# tabling

This folder contains an example of using tabled predicates within objects.
Currently supported compilers include B-Prolog, XSB, SWI-Prolog (when the
`tabling` library is available), and YAP (when compiled with tabling enabled).

Current tabling implementations don't provide a solution for ignoring the
implicit execution-context argument that the Logtalk compiler adds to all
compiled predicates. Thus, different object sending a message for a tabled
predicate will result in the equivalent of multiple tables. The same will
happen if the sender of the message is a parametric object and different
parameterizations are used. A possible workaround is to always send the
message from the same (non-parametric) object (e.g., `user`).

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(tabling(loader)).
```

Use tabling to cope with a left-recursive path finding predicate
(the order of the solutions may depend on the tabling strategy):

```logtalk
paths::path(1, Y).
```

<!--
Y = 2 ? ;
Y = 4 ? ;
Y = 3 ? ;
Y = 5 ? ;
false.
-->

Use tabling to avoid repeated calculation of Fibonacci numbers:

```logtalk
fibonacci::fib(30, F).
```

<!--
F = 1346269.
-->
