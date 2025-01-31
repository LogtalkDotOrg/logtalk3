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

# lambdas_compiled

This is a pseudo-example, based on the `lambdas` example, that is
only used for testing library support for compilation of calls to
meta-predicates and lambda expressions that removes the implied
overhead making their performance practical and equal or close to
the first-order alternatives.

Run the tests:

```logtalk
logtalk_load(lambdas_compiled(tester)).
```

The following lambda benchmarks are so far only available when using
SWI-Prolog, Trealla Prolog, XSB, XVM, or YAP as the Logtalk backend
compilers:

```logtalk
lambda_benchmarks::bench1.
```

<!--
Using map/2 with a closure for testing less(0, X) with X in [1..100000]: 
% 200,003 inferences, 0.015 CPU in 0.018 seconds (83% CPU, 13027814 Lips)
Using map/2 with a lambda for testing less(0, X) with X in [1..100000]:  
% 300,002 inferences, 0.010 CPU in 0.010 seconds (97% CPU, 31059323 Lips)

true.
-->

```logtalk
lambda_benchmarks::bench2.
```

<!--
Adding 1 to every integer in the list [1..100000] using a local add1/2 predicate:
% 100,002 inferences, 0.014 CPU in 0.015 seconds (93% CPU, 6909556 Lips)
Adding 1 to every integer in the list [1..100000] using map/3 with the integer::plus/3 predicate:
% 200,004 inferences, 0.047 CPU in 0.047 seconds (98% CPU, 4296911 Lips)
Adding 1 to every integer in the list [1..100000] using map/3 with a lambda argument with a is/2 goal:
% 200,002 inferences, 0.019 CPU in 0.019 seconds (98% CPU, 10666204 Lips)

true.
-->
