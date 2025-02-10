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

# futures

This is a simple example of defining and using _futures_, a common concurrent
programming idiom. A future is used to represent a term that may not yet be
available due to a pending asynchronous computation. Futures are supported
in Logtalk using its high-level multi-threading features. This example uses
computations over lists as representative of long running operations that
would benefit from asynchronous calls.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(futures(loader)).
```

Use the asynchronous calls based predicate definition:

```logtalk
integer::sequence(1,100,List1), integer::sequence(101,200,List2), list_math::product_sum(List1,List2,Result).
```

<!--
Result = 76002500.
-->

Use the threaded engines based predicate definition:

```logtalk
integer::sequence(1,100,List1), integer::sequence(101,200,List2), list_math::product_sum_engines(List1,List2,Result).
```

<!--
Result = 76002500.
-->
