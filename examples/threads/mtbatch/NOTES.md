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

# threads - mtbatch

This folder provides an object for running multi-threading benchmarks.

The following tests *independent and-parallelism* are available, based on
individual multi-threading examples:

- `primes` (defaults to 16 threads, starting list size of 10000, and 10 multiples)
- `primes(MaxThreads, StartSize, Multiples)`

- `msort` (defaults to 16 threads, starting list size 5000, and 10 multiples)
- `msort(MaxThreads, Size, Multiples)`

- `qsort` (defaults to 16 threads, starting list size 5000, and 10 multiples)
- `qsort(MaxThreads, Size, Multiples)`

- `fib` (defaults to 16 threads and the [20,27] interval)
- `fib(MaxThreads, Lower, Upper)`

- `hanoi` (defaults to 16 threads and the [20,27] interval)
- `hanoi(MaxThreads, Lower, Upper)`

- `tak` (defaults to 243 threads and the [7,11] interval)
- `tak(MaxThreads, Lower, Upper)` (use a power of 3 as argument: 1, 3, 9, 27, 81, ...)

- `fft` (defaults to 16 threads and the [10,16] interval)
- `fft(MaxThreads, Lower, Upper)`

- `integration` (defaults to 16 threads)
- `integration(MaxThreads)`

- `integration2d` (defaults to 16 threads)
- `integration2d(MaxThreads)` (use a power of 4 as argument: 1, 4, 16, 64, 256, ...)

There's also a test of *competitive or-parallelism* based on the `searching`
example:

- `search`

For the same backend Prolog compiler, the benchmark results can show
significant variation depending on the operating-system and if you're
using a 32 bits or a 64 bits version.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(mtbatch(loader)).
```

Run all tests the default number of times (10):

```logtalk
mtbatch::run.
```

Run all tests (average of 20 times for each test):

```logtalk
mtbatch::run(20).
```

Run only the primes benchmark test (average of 10 times):

```logtalk
mtbatch::run(primes, 10).
```
