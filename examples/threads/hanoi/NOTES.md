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

# threads - hanoi

This folder provides a multi-threading solution for solving the "Towers
of Hanoi" problem. This solution is only useful for benchmarking as the 
problem is solved without actually printing the solution steps.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(hanoi(loader)).
```

NOTE: some example queries below use a proprietary predicate `time/1` in
order to get accurate goal times. This predicate is found on several Prolog
systems. For other Prolog compilers, replace the `time/1` call by any
appropriate timing calls (e.g., `cputime/0`).

Solve the Towers of Hanoi problem for 24 disks using a single thread:

```logtalk
time(hanoi(1)::run(24)).
```

<!--
% 25,165,864 inferences, 4.94 CPU in 5.12 seconds (96% CPU, 5094304 Lips)

true.
-->

Solve the Towers of Hanoi problem for 24 disks using two threads:

```logtalk
time(hanoi(2)::run(24)).
```

<!--
% 78 inferences, 4.87 CPU in 2.66 seconds (183% CPU, 16 Lips)

true.
-->

Solve the Towers of Hanoi problem for 24 disks using four threads:

```logtalk
time(hanoi(4)::run(24)).
```

<!--
% 78 inferences, 4.88 CPU in 2.60 seconds (187% CPU, 16 Lips)

true.
-->

Solve the Towers of Hanoi problem for 24 disks using eight threads:

```logtalk
time(hanoi(8)::run(24)).
```

<!--
% 78 inferences, 4.98 CPU in 1.55 seconds (321% CPU, 16 Lips)

true.
-->

Solve the Towers of Hanoi problem for 24 disks using sixteen threads:

```logtalk
time(hanoi(16)::run(24)).
```

<!--
% 78 inferences, 5.03 CPU in 1.44 seconds (348% CPU, 16 Lips)

true.
-->
