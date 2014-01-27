________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
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
FOR NP=1,2,3,4 an adaptive gaussian quadrature of 1, 2, 3, or points is used.

For `quadrec/1`, the method used for the multithreading is simply to divide the 
initial area amongst the number of threads available (a power of 2) and then 
in each interval the recursive method is applied. The `threaded/1` predicate 
is used.

For `quadsplit/1`, the method used is again division (split) of the original
area amongst the number of threads specified. This method has no restriction
on the number of threads and uses a span/collect idea for proving thread goals 
and the predicates `threaded_once/1` and `threaded_exit/1`.
