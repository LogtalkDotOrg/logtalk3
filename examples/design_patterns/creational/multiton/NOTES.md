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
	Multiton

Description:
	Generalization of the Singleton design pattern where a fixed number
	of named instances of a class is managed.

This pattern can be used with both classes and prototypes. The description
above is from the Wikipedia page on this pattern:

https://en.wikipedia.org/wiki/Multiton_pattern

This pattern is not described in the GoF book. See the Wikipedia page for
details and references.

The sample implementation uses classes. To simplify, it assumes a fixed
(at compile-time) set of named instances. An alternative would be allow
the definition of the set of named instances at runtime by extending the
protocol of the multiton with the necessary predicates. Also to simplify,
the sample code uses the named instance identifiers as the access keys.
This could also be changed to use instead a dictionary.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('creational/multiton/loader')).
```

Get a list of all named instances:

```logtalk
multiton::instances(Instances).
```

<!--
Instances = [i1, i2, i3].
-->

Verify that no named instance exists until is actually requested:

```logtalk
\+ current_object(i1), \+ current_object(i2), \+ current_object(i3).
```

<!--
true.
-->

Request to the multiton class one of its named instances:

```logtalk
multiton::instance(i2).
```

<!--
true.
-->

```logtalk
current_object(i2).
```

<!--
true.
-->

Try to request an instance that is not a member of the named instances:

```logtalk
multiton::instance(foobar).
```

<!--
false.
-->

```logtalk
current_object(foobar).
```

<!--
false.
-->
