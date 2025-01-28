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

# object_aliases

This is a minimal example illustrating the use of the `uses/1` directive
to experiment with different object implementations of the same protocol
when using explicit message-sending. The main idea is to have a single
source line that can be edited to switch between different implementations
as all message-sending calls would be written using the object alias.
This is an alternative to accomplish the same goal by using a `uses/2`
directive and implicit message-sending. Note that both alternatives allow
compiling the code in optimized mode to take advantage of static binding
for the message-sending calls.

This example also illustrates defining an alias for an object defined at
runtime using a _parameter variable_.

Start by loading the example:

```logtalk
logtalk_load(object_aliases(loader)).
```

Demonstration of using aliases for shorten object names and
simplify trying alternative implementations:

```logtalk
experiments::stats(TotalLess, TotalEqual, TotalGreater), Total is TotalLess + TotalEqual + TotalGreater.
```

<!--
Total = 42.
-->

Demonstration of using aliases for objects defined at runtime
using parameters:

```logtalk
simple(<)::insert_top([3-c,1-a,2-b], Top).
```

<!--
Top = 1-a.
-->

```logtalk
simple(>)::insert_top([3-c,1-a,2-b], Top).
```

<!--
Top = 3-c.
-->
