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

# debug_hooks

This folder contains a very simple example of using the compiler flag
`hook/1` and the term expansion mechanism to either discard or activate
debugging goals. For debugging goals in clause bodies, one defines clauses
for `goal_expansion/2`. For debugging goals in directives (e.g., in the
`initialization/1` directive), one defines clauses for `term_expansion/2`.

We can use two hook objects, one for developing and debugging code and
one for production code, or a single parametric object. This example
illustrates both approaches although only the first one is used in the
loader auxiliary files and in the sample queries.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example with the debug statements activated:

```logtalk
logtalk_load(debug_hooks(loader_debug)).
```

Debug the definition of the `object::append/3` predicate:

```logtalk
object::append([1,2,3], [4,5], List).
```

<!--
Recursive case: append([2, 3], [4, 5], _G340)
Recursive case: append([3], [4, 5], _G347)
Recursive case: append([], [4, 5], _G354)
Base case: append([], [4, 5], [4, 5])

List = [1, 2, 3, 4, 5].
-->

Debug calls to the `object::sum/2` predicate:

```logtalk
object::sum([1,2,3,_], Sum).
```

<!--
Exception: error(instantiation_error, number::check(_G433), object)
-->

```logtalk
object::sum([1,2,3,a], Sum).
```

<!--
Exception: error(type_error(number, a), number::check(a), object)
-->

```logtalk
object::sum(wrong, Sum).
```

<!--
Exception: error(type_error(list, wrong), list::check(wrong), object)
-->

Reload the example with the debug statements discarded:

```logtalk
logtalk_load(debug_hooks(loader_production)).
```

Call the `object::append/3` predicate without the debugging statements:

```logtalk
object::append([1,2,3], [4,5], List).
```

<!--
List = [1, 2, 3, 4, 5].
-->

Call the `object::sum/2` predicate without the debugging statements:

```logtalk
object::sum([1,2,3,_], Sum).
```

<!--
Exception: instantiation_error
-->

```logtalk
object::sum([1,2,3,a], Sum).
```

<!--
Exception: type_error(evaluable, a/0)
-->

```logtalk
object::sum(wrong, Sum).
```

<!--
false.
-->
