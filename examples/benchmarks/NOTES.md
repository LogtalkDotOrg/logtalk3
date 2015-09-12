________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>

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

This folder provides simple benchmark tests for comparing Logtalk message 
sending performance with direct predicates calls in plain Prolog.
These benchmarks may also be used for comparing Logtalk message sending 
performance across Prolog compilers.

This example is made of four loader files and five source files:

- `loader_events.lgt`  
	loads all source files with event support turned on
- `loader_no_events.lgt`  
	loads all source files with event support turned off
- `loader.lgt`  
	the same as the `loader_no_events.lgt` file

- `benchmarks.lgt`  
	contains the benchmark goals and testing predicates
- `plain.lgt`  
	contains definitions for list length and list reverse predicates
	and a predicate for testing performance of the built-in predicates
	`assertz/1` and `retract/1`
- `module.pl` (not loaded by default; see below)  
	contains the same definitions of the list length and list reverse
	predicates encapsulated in a module
- `objects.lgt`  
	contains an object encapsulating the same definitions of list length
	and list reverse predicates, plus two descendant objects to simulate
	a small hierarchy (used for testing calls to imported category
	predicates)
- `database.lgt` and `database_other.lgt`  
	contains predicates for testing the performance of the built-in 
	database methods `assertz/1` and `retract/1`
- `category.lgt`  
	contains a single predicate used when comparing performance of
	calls to imported category predicates using direct calls and using 
	messages to "self"
- `maze.lgt`  
	contains a simple maze search problem using a depth-first strategy
	with loop detection

You may have noticed above that the benchmark predicates and the predicates 
for plain Prolog testing are both encapsulated in Logtalk source files. The 
Logtalk compiler just copies the plain Prolog code to the generated Prolog 
files. The reason for using the `.lgt` extension for these files is simply
to  make it possible to load the example code using calls to the predicates 
`logtalk_load/1-2`.

By default, the benchmark tests on the `SCRIPT.txt` file use a list of 30
elements as an argument to the list length and list reverse predicates. When
dynamic binding is used, increasing the list length results in decreasing
performance differences between plain Prolog and Logtalk as the length and
reverse computation times far outweigh the overhead of the message sending
mechanism. Likewise, decreasing the list length leads to increasing performance
differences between plain Prolog and Logtalk (up to the point you will be
measuring the Logtalk message sending mechanism overhead compared to plain
Prolog predicate calls). In real-life applications, only testing can give
you a balanced view on the trade-offs between plain Prolog performance and
Logtalk programming features.

The loader files load a module version of the tests when a back-end Prolog
compiler that supports modules is used. For most Prolog module systems, the 
performance of module calls is close or even identical to the performance of 
plain Prolog calls when using imported predicates and implicit qualification.
When using explicit module qualification, performance can be worse.

When static binding is used, messages to objects are, whenever possible, 
translated to direct predicate calls. Thus performance should be about the 
same as in plain Prolog predicate calls. However, due to the overhead of 
one extra argument per object predicate (used for passing the execution 
context), the performance of Logtalk optimized calls might be slightly 
worse than the equivalent plain Prolog predicate calls.

When running the tests, pay special attention to the empty loop times. If
the times for the empty loop are not stable across runs, try to use an
higher value (than the default one) as argument of the `run/1` predicate.

When the number of benchmark test repetitions is too low, you may get a
"Number of calls per second: n/a" result.

The test that creates and abolishes empty objects may take a long time to
complete when compared with all the other tests.

