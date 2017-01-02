________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

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
functions where the splitting leaves must of work to just a few threads 
(resulting in load balancing problems).

The split/span/collect of thread goals uses the Logtalk built-in predicates 
`threaded_once/1` and `threaded_exit/1`.

Both objects implement the same protocol:

    integrate(Function, A, B, C, D, NP, Error, Integral)

This predicate allows us to find the integral of a function of two variables 
on the rectangular domain `[A,B][C,D]` given a maximum approximation error.
NP represents the method to be used (0,1,2,3). For NP = 0, an adaptive 
trapezoidal rule is used. For NP = 1, 2, 3 an adaptive gaussian quadrature of
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
