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

# threads - integration2d

This folder contains a multi-threading implementation of Recursive Gaussian 
Quadrature Methods for Numerical Integration for functions of two variables.

Adaptive quadrature methods are efficient numerical integration techniques
as they compensate for functional variation along the integral domain. This
is accomplished by using a larger sampling of points in regions with large 
function variations.

Two objects, `quadrec2d` and `quadsplit2d`, implement two versions of the 
quadrature methods.

In the `quadrec2d` object, the multi-threading method used is to divide 
the integral volume amongst the number of threads used (a power of 4) and 
then recursively apply the same method in each interval. The implementation 
uses the `threaded/1` Logtalk built-in predicate.

In the `quadsplit2d` object, a divide and conquer approach is also used but 
the original domain is split along a single dimension. A thread is spawned 
for each sub-interval. This method can use any number of threads. However, 
by splitting along a single dimension, this method may perform poorly for 
functions where the splitting leaves most of work to just a few threads 
(resulting in load balancing problems).

The split/span/collect of thread goals uses the Logtalk built-in predicates 
`threaded_once/1` and `threaded_exit/1`.

Both objects implement the same protocol:

	integrate(Function, A, B, C, D, NP, Error, Integral)

This predicate allows us to find the integral of a function of two variables 
on the rectangular domain `[A,B][C,D]` given a maximum approximation error.
NP represents the method to be used (0,1,2,3). For NP = 0, an adaptive 
trapezoidal rule is used. For NP = 1, 2, 3 an adaptive Gaussian quadrature of
1, 2, or 3 points is used.


The 2D recursive algorithm and the example functions i14 and i15 are described
in the following article:

Wilhelm M. Pieper, "Recursive Gauss integration",
Communications in Numerical Methods in Engineering, Vol. 15(2) 1999, pp 77-90.
http://www3.interscience.wiley.com/journal/45002124/abstract?CRETRY=1&SRETRY=0

The example functions bailey1, bailey2, bailey3, bailey4, and bailey5 are from
the following article:

David H. Bailey and Jonathan M. Borwein, 
"Highly Parallel, High-Precision Numerical Integration" (April 22, 2005).
Lawrence Berkeley National Laboratory. Paper LBNL-57491.
http://repositories.cdlib.org/lbnl/LBNL-57491

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(integration2d(loader)).
```

Integrate the function `circle` using the 2d split&spawn adaptive quadrature method:

```logtalk
time(quadsplit2d(1)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral)).
```

<!--
% 35,302,093 inferences, 19.83 CPU in 20.52 seconds (97% CPU, 1780237 Lips)
Integral = -21.3333.
-->

```logtalk
time(quadsplit2d(4)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral)).
```

<!--
% 119 inferences, 20.27 CPU in 5.57 seconds (364% CPU, 6 Lips)
Integral = -21.3333.
-->

```logtalk
time(quadsplit2d(16)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral)).
```

<!--
% 383 inferences, 84.60 CPU in 13.53 seconds (625% CPU, 5 Lips)
Integral = -21.3333.
-->

Integrate the function `circle` using the 2d recursive adaptive quadrature method:

```logtalk
time(quadrec2d(1)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral)).
```

<!--
% 35,302,078 inferences, 19.90 CPU in 20.59 seconds (97% CPU, 1773974 Lips)
Integral = -21.3333.
-->

```logtalk
time(quadrec2d(4)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral)).
```

<!--
% 229 inferences, 20.28 CPU in 5.56 seconds (365% CPU, 11 Lips)
Integral = -21.3333.
-->

```logtalk
time(quadrec2d(16)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral)).
```

<!--
% 228 inferences, 21.14 CPU in 3.40 seconds (622% CPU, 11 Lips)
Integral = -21.3333.
-->

Integrate the function `i15` using the 2d recursive adaptive quadrature method:

```logtalk
time(quadrec2d(1)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral)).
```

<!--
% 4,754,844 inferences, 2.71 CPU in 2.82 seconds (96% CPU, 1754555 Lips)
Integral = 7.73592.
-->

```logtalk
time(quadrec2d(4)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral)).
```

<!--
% 229 inferences, 2.77 CPU in 0.77 seconds (360% CPU, 83 Lips)
Integral = 7.73592.
-->

```logtalk
time(quadrec2d(16)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral)).
```

<!--
% 229 inferences, 2.88 CPU in 0.51 seconds (562% CPU, 80 Lips)
Integral = 7.73592.
-->

NOTE for Testing.

Single Threaded - All Examples using the quadrec2d object

time(quadrec2d(1)::integrate( circle, 	-2, 2, -2, 2, 3, 1e-5, Integral)).
time(quadrec2d(1)::integrate( i14,	 	-2, 2, -2, 2, 3, 1e-5, Integral)).
time(quadrec2d(1)::integrate( i15,	 	-2, 2, -2, 2, 3, 1e-5, Integral)).

time(quadrec2d(1)::integrate( bailey1, 0.0, 1.0, 0.0, 1.0, 3, 1e-5, Integral)).
time(quadrec2d(1)::integrate( bailey2, 0.0, 1.0, 0.0, 1.0, 3, 1e-5, Integral)).
time(quadrec2d(1)::integrate( bailey3, -1.0, 1.0, -1.0, 1.0, 3, 1e-5, Integral)).


More difficult cases:

time(quadrec2d(1)::integrate( bailey4, 1.0e-6, pi, 0.0, pi,3, 1e-3, Integral)).

time(quadrec2d(1)::integrate( bailey5, 0.0, 100, 0.0, 100, 3, 1e-6, Integral)).


Single Threaded - All Examples using the quadsplit2d object

time(quadsplit2d(1)::integrate( circle, 	-2, 2, -2, 2, 3, 1e-5, Integral)).
time(quadsplit2d(1)::integrate( i14,	 	-2, 2, -2, 2, 3, 1e-5, Integral)).
time(quadsplit2d(1)::integrate( i15,	 	-2, 2, -2, 2, 3, 1e-5, Integral)).

time(quadsplit2d(1)::integrate( bailey1, 0.0, 1.0, 0.0, 1.0, 3, 1e-5, Integral)).
time(quadsplit2d(1)::integrate( bailey2, 0.0, 1.0, 0.0, 1.0, 3, 1e-5, Integral)).
time(quadsplit2d(1)::integrate( bailey3, -1.0, 1.0, -1.0, 1.0, 3, 1e-5, Integral)).


More difficult cases:

time(quadsplit2d(1)::integrate( bailey4, 1.0e-6, pi, 0.0, pi, 3, 1e-3, Integral)).

time(quadsplit2d(1)::integrate( bailey5, 0.0, 100, 0.0, 100, 3, 1e-6, Integral)).
