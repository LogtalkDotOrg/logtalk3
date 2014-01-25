________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt` file.

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
(i.e. to compensate the overhead of thread creation and management).
