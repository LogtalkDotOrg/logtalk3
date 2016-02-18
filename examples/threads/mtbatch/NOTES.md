________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

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

This folder provides an object for running multi-threading benchmarks. The
supported back-end Prolog compilers are	SWI-Prolog, YAP, and XSB.

For example, the following goal will run all benchmark tests:

	| ?- mtbatch::run.

You may also run just a single benchmark test a given number of times.
For example:

	| ?- mtbatch::run(primes, 10).

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

For the same back-end Prolog compiler, the benchmark results can show 
significant variation depending on the operating-system and if you're 
using a 32 bits or a 64 bits version.
