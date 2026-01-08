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

# closed_world_assumption

This example illustrates the difference between *declaring* a predicate and
*defining* a predicate and the Closed World Assumption (CWA) semantics as
implemented in Logtalk when calling predicates and sending messages.

The CWA states that:

- What is true is known to be true (i.e., what is true can be proved).
- What is not know to be true, is false (i.e., what cannot be proved true,
is false).

When applied to Logtalk (or Prolog), proofs are constructed using predicate
definitions. But Logtalk also provides a clear distinction between declaring
a predicate and defining a predicate (1). This distinction is translates to
the following refined CWA semantics:

- Messages or calls for declared but undefined predicates fail.
- Messages or calls for unknown (not declared) predicates throw an error.

The Logtalk linter reports whenever possible the cases that will likely
result in runtime error.

(1) This is a necessary requirement for protocols/interfaces: we must be
able to declare a predicate without necessarily defining it.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example (by default, it will generate a number of
warnings, which are expected):

```logtalk
logtalk_load(closed_world_assumption(loader)).
```

`attic/0` is a public predicate, defined to be true:

```logtalk
house::attic.
```

<!--
true.
-->

`cellar/0` is also a public predicate but without a definition; the
message will fail per closed world assumption:

```logtalk
house::cellar.
```

<!--
false.
-->

No `pool/0` predicate declared; closed world assumption doesn't apply
and thus the message generates an error:

```logtalk
house::pool.
```

<!--
ERROR: error(existence_error(predicate_declaration,pool/0), logtalk(house::pool, _))
-->

Our house is pleasant as we have a porch and a garden:

```logtalk
house::pleasant.
```

<!--
true.
-->

But our house is not practical as we don't have a shed for the garden
tools or a garage to protect our bikes (per closed world assumption,
calling declared by undefined predicates fail):

```logtalk
house::practical.
```

<!--
false.
-->

No idea if we live in a fun house, however, as calling a undeclared and
undefined predicate generates an error; the closed world assumption
doesn't apply in this case:

```logtalk
house::fun.
```

<!--
ERROR: error(existence_error(procedure,pool/0), logtalk(house::fun, _))
-->
