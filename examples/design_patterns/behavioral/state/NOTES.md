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

Design pattern:
	State

Description:
	"Allow an object to alter its behavior when its internal state
	changes. The object will appear to change its class."

This pattern can be used with both classes and prototypes.

We use prototypes in this sample implementation. The context object
uses a dynamic predicate to hold the current concrete state object
identifier with a public predicate to set it. The concrete state
objects perform the context-switching after handling a forwarded
client request.

Start by loading the design pattern sample implementations:

```logtalk
logtalk_load(design_patterns('behavioral/state/loader')).
```

Sequence of client messages:

```logtalk
context::output(1 + 2).
```

<!--
1+2

true.
-->

```logtalk
context::output(6 / 7).
```

<!--
/(6,7)

true.
-->

```logtalk
context::output(3 * 4 + 5).
```

<!--
3*4+5

true.
-->

```logtalk
context::output(1 - 2 / 7).
```

<!--
-(1,/(2,7))

true.
-->
