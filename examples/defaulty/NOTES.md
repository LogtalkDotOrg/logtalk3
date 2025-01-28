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

# defaulty

This example compares defaulty and tagged data representations using the
`ports_profiler` tool. The example defines both `defaulty` and `tagged`
objects implementing the same predicate. This predicate, `count_atomics/3`,
counts the number of atoms and the number of numbers in a list that can
contain atoms, numbers, and other types of terms that are irrelevant.
Running the same query for both representations, the `ports_profiler` tool
is used to highlight their performance difference.

For a detailed analysis of this example, see the following blog post:

https://logtalk.org/2019/12/17/the-cost-of-defaulty-representations.html


Start by loading the example and the `ports_profiler` tool:

```logtalk
logtalk_load(defaulty(loader)).
```

Get ports profiling data for both defaulty and tagged representations:

```logtalk
defaulty::count_atomics([a,1,_,b,2,_,c,3,_], As, Ns).
```

<!--
As = Ns, Ns = 3.
-->

Print the profiling data:

```logtalk
ports_profiler::data.
```

<!--
---------------------------------------------------------------------------
Entity    Predicate          Fact  Rule  Call  Exit *Exit  Fail  Redo Error
---------------------------------------------------------------------------
defaulty  count_atomic/5        3    15     9     9     0     0     0     0
defaulty  count_atomics/3       0     1     1     1     0     0     0     0
defaulty  count_atomics/5       1     9    10    10     0     0     0     0
---------------------------------------------------------------------------

true.
-->

Reset the profiling data for the next query:

```logtalk
ports_profiler::reset.
```

```logtalk
tagged::count_atomics([a(a),n(1),o(_),a(b),n(2),o(_),a(c),n(3),o(_)], As, Ns).
```

<!--
As = Ns, Ns = 3.
-->

```logtalk
ports_profiler::data.
```

<!--
-------------------------------------------------------------------------
Entity  Predicate          Fact  Rule  Call  Exit *Exit  Fail  Redo Error
-------------------------------------------------------------------------
tagged  count_atomic/5        3     6     9     9     0     0     0     0
tagged  count_atomics/3       0     1     1     1     0     0     0     0
tagged  count_atomics/5       1     9    10    10     0     0     0     0
-------------------------------------------------------------------------

true.
-->
