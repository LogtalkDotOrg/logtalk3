---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
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

# threads - mtbatch

This folder provides an object for running multi-threading benchmarks. The
supported backend Prolog compilers are SWI-Prolog, and YAP.

The following tests are available:

	primes			(independent and-parallelism)
	msort			(independent and-parallelism)
	qsort			(independent and-parallelism)
	fib				(independent and-parallelism)
	hanoi			(independent and-parallelism)
	tak				(independent and-parallelism)
	fft				(independent and-parallelism)
	integration		(independent and-parallelism)
	integration2d	(independent and-parallelism)
	search			(competitive or-parallelism)

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

Run all tests:

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
