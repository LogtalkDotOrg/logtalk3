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

# threads - fibonacci

This folder provides a multi-threading solution for calculating Fibonacci
numbers. This solution is only useful for benchmarking.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(fibonacci(loader)).
```

NOTE: some example queries below use a proprietary predicate `time/1` in
order to get accurate goal times. This predicate is found on several Prolog
systems. For other Prolog compilers, replace the `time/1` call by any
appropriate timing calls (e.g., `cputime/0`).

Calculate 24 Fibonacci number using a single thread:

```logtalk
time(fibonacci(1)::fib(24, N)).
```

<!--
% 450,175 inferences, 0.20 CPU in 0.23 seconds (88% CPU, 2250875 Lips)

N = 75025.
-->

Calculate the 24 Fibonacci number using two threads:

```logtalk
time(fibonacci(2)::fib(24, N)).
```

<!--
% 81 inferences, 0.18 CPU in 0.14 seconds (131% CPU, 450 Lips)

N = 75025.
-->

Calculate the 24 Fibonacci number using four threads:

```logtalk
time(fibonacci(4)::fib(24, N)).
```

<!--
% 81 inferences, 0.17 CPU in 0.14 seconds (124% CPU, 476 Lips)

N = 75025.
-->

Calculate the 24 Fibonacci number using eight threads:

```logtalk
time(fibonacci(8)::fib(24, N)).
```

<!--
% 81 inferences, 0.16 CPU in 0.09 seconds (181% CPU, 506 Lips)

N = 75025.
-->

Calculate the 24 Fibonacci number using sixteen threads:

```logtalk
time(fibonacci(16)::fib(24, N)).
```

<!--
% 81 inferences, 0.16 CPU in 0.08 seconds (206% CPU, 506 Lips)

N = 75025.
-->
