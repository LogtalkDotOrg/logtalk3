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

# pardicts

This example requires using SWI-Prolog 7.x as the backend Prolog compiler.
It illustrates using a SWI-Prolog native dictionary term for representing
a parametric object parameters. This is accomplished simply by passing a
dict as the single object parameter.

## Know issue

Don't use dot notation, `./2`, when working with dicts within objects and
categories. SWI-Prolog provides different semantics for compiled versus
asserted clauses that contain `./2` terms and that can clash with Logtalk
dynamic binding caching and inlining optimizations (usually resulting
in instantiation errors).

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(pardicts(loader)).
```

Access individual parameters:

```logtalk
obj(_{m:2,n:3})::sum(Sum).
```

<!--
Sum == 5.
-->

```logtalk
obj(_{m:2,n:3})::product(Sum).
```

<!--
Sum == 6.
-->

Update individual parameters:

```logtalk
Dict = _{m:2,n:3}, obj(Dict)::double.
```

<!--
Dict = _2524{m:4, n:6}.
-->
