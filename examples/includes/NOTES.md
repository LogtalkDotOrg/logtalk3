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

# includes

This example illustrates using the `include/1` directive as both a file
directive and an entity directive. It also illustrates how to use this
directive to implement a simple persistency solution for an object
database.

Start by loading the example:

```logtalk
logtalk_load(includes(loader)).
```

Test the use of the Logtalk `include/1` directive as a file directive:

```logtalk
findall(Vowel, vowel(Vowel), Vowels).
```

<!--
Vowels = [a,e,i,o,u].
-->

Test the use of the Logtalk `include/1` directive as an entity directive
by getting a list of all capitals:

```logtalk
countries::capitals(Capitals).
```

<!--
Capitals = [berlim, lisbon, madrid, paris, varsovia].
-->

Test the use of the Logtalk `include/1` directive as an entity directive
by getting sets of countries with the same population:

```logtalk
setof(Countries, countries::same_population(Countries), Solutions).
```

<!--
Solutions = [[france, poland], [germany, spain], [portugal]].
-->

Show the initial counter values, which may depend on previous runs:

```logtalk
counters::counter(Counter, Value).
```

<!--
Counter = a,
Value = ... ;
Counter = b,
Value = ... ;
Counter = c,
Value = ... ;
false.
-->

Increment one of the counters:

```logtalk
counters::inc_counter(b).
```

<!--
true.
-->

Save the counters database:

```logtalk
counters::save_counters.
```

<!--
true.
-->
