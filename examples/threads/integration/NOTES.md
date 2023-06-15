________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This folder contains a multi-threading implementation of Recursive Gaussian 
Quadrature Methods for Numerical Integration for functions of one-variable.

Adaptive quadrature methods are efficient techniques for numerical 
integration as they compensate for functional variation along the 
integral domain, effectively in regions with large function variations 
a larger sampling of point are used.

There are two parametric objects, `quadrec/1` and `quadsplit/1`, both
implementing the same integration predicate:

	integrate(Function, Left, Right, NP, Epsilon, Integral)

Find the integral of a function of one variable in the interval `[Left, Right]`
given a maximum approximation error. `NP` represents the method to be used, one
of (0,1,2,3).

For NP = 0 an adaptive trapezoidal rule is used.
FOR NP=1,2,3,4 an adaptive Gaussian quadrature of 1, 2, 3, or points is used.

For `quadrec/1`, the method used for the multi-threading is simply to divide
the initial area amongst the number of threads available (a power of 2) and
then in each interval the recursive method is applied. The `threaded/1`
predicate is used.

For `quadsplit/1`, the method used is again division (split) of the original
area amongst the number of threads specified. This method has no restriction
on the number of threads and uses a span/collect idea for proving thread goals 
and the predicates `threaded_once/1` and `threaded_exit/1`.
