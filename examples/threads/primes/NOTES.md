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

# threads - primes

This folder contains a simple example of calculating prime numbers in a 
given interval using multiple threads. Try to run the example in single 
and multi-processor (or multi-core) computers and compare the results. 
Most Prolog compilers allows you to measure the time taken for proving 
a goal using proprietary predicates.

Note that this example is only meant to illustrate how to use Logtalk 
multi-threading predicates, not to taken as the efficient solution for 
finding primes numbers on a given interval (with or without threads).

You probably want to play with the list size in order to find out when the 
list is big enough to make the use of multi-threading worth performance-wise 
(i.e., to compensate the overhead of thread creation and management).

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(primes(loader)).
```

NOTE: some example queries below use a proprietary predicate `time/1` in
order to get accurate goal times. This predicate is found on several Prolog
systems. For other Prolog compilers, replace the `time/1` call by any
appropriate timing calls (e.g., `cputime/0`).

Calculate the prime numbers in a given interval using a single thread:

```logtalk
time(primes(1)::primes(1, 500000, Primes)).
```

<!--
% 67,657,303 inferences, 11.98 CPU in 12.31 seconds (97% CPU, 5647521 Lips)

Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23|...].
-->

Calculate the prime numbers in a given interval by splitting the interval 
in two sub-intervals and using a thread per sub-interval:

```logtalk
time(primes(2)::primes(1, 500000, Primes)).
```

<!--
% 77 inferences, 11.73 CPU in 7.48 seconds (157% CPU, 7 Lips)

Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23|...].
-->

Calculate the prime numbers in a given interval by splitting the interval 
in four sub-intervals and using a thread per sub-interval:

```logtalk
time(primes(4)::primes(1, 500000, Primes)).
```

<!--
% 143 inferences, 11.62 CPU in 4.00 seconds (290% CPU, 12 Lips)

Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23|...].
-->

Calculate the prime numbers in a given interval by splitting the interval 
in eight sub-intervals and using a thread per sub-interval:

```logtalk
time(primes(8)::primes(1, 500000, Primes)).
```

<!--
% 323 inferences, 11.56 CPU in 3.30 seconds (350% CPU, 28 Lips)

Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23|...].
-->
