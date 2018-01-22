________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

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
