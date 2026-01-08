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

Design pattern:
	Chain of Responsibility

Description:
	"Avoid coupling the sender of a request to its receiver by giving
	more than one object a chance to handle the request. Chain the
	receiving objects and pass the request along the chain until an
	object handles it."

This pattern can be used with both classes and prototypes.

In this sample implementation, we use prototypes and fixed successors
in the chain of responsibility. But the successors could also easily
be defined dynamically.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/chain_of_responsibility/loader')).
```

Try some purchasing requests of varying amount:

```logtalk
manager::process_request(3500).
```

<!--
Manager will approve 3500
true.
-->

```logtalk
manager::process_request(6500).
```

<!--
Director will approve 6500
true.
-->

```logtalk
manager::process_request(11500).
```

<!--
Vice President will approve 11500
true.
-->

```logtalk
manager::process_request(27000).
```

<!--
President will approve 27000
true.
-->

```logtalk
manager::process_request(32000).
```

<!--
Request denied for 32000
false.
-->
