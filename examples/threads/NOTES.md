________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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


This folder contains several examples of multi-threading programming.
Multi-threading programming is only supported on some Prolog compilers.
Currently this includes SWI-Prolog, YAP, and some XSB versions (make sure
that you use the multi-threading versions of these Prolog compilers!). 
Moreover, multi-threading may be turned off by default. In order to run 
the examples,  you may need to first turn on multi-threading support on
the Prolog adapter files.

Some examples try to benchmark single-threaded and multi-threaded solutions.
Depending on the Prolog compiler, the operating-system, and the computer
used, you may need to adjust the size of the problem data in order to find
the threshold where multi-threading solutions begin to outperform the
single-threaded solutions.

Some examples may imply adjusting the default size of thread data areas or,
preferably, use of the 64 bits version of the compatible Prolog compilers.

There are known Prolog bugs on the multi-threading support found on XSB 
and YAP. These bugs prevent some examples to run and may lead to crashes.
Some bugs are platform-specific, only occurring on some operating-systems.

Follows a short description of the included example (in alphabetical order):

- `barriers`
	barrier synchronization using threaded notifications
- `birthdays`
	using threads to represent agents
- `blackboard`
	synchronization of threads using shared resources
- `buckets`
	atomic updates example
- `buffer`
	synchronizing threads writing to and reading from a buffer
- `checkpoint`
	using a barrier as a checkpoint to synchronize a set of worker
	threads assembling a set of items
- `fft`
	multi-threading computation of the Fast Fourier Transform
- `fibonacci`
	multi-threading solution for computing Fibonacci numbers
- `functions`
	competitive or-parallelism computation of functions
- `hanoi`
	multi-threading version of the "Towers of Hanoi" problem
- `integration`
	multi-threading implementation of recursive Gaussian quadrature
	methods for numerical integration of functions of one variable
- `integration2d`
	multi-threading implementation of recursive Gaussian quadrature
	methods for numerical integration of functions of two variables
- `metered_concurrency`
	an implementation of a metered concurrency task
- `mtbatch`
	multi-threading benchmarks for and-parallelism and competitive
	or-parallelism
- `nondet`
	non-deterministic multi-threading calls
- `philosophers`
	classical "dining philosophers" problem using threads
- `ping_pong`
	two threads playing a ping-pong game
- `primes`
	multi-threading computation of prime numbers in a given interval
- `sorting`
	multi-threading implementation of the merge sort algorithm
- `sync`
	synchronization of predicates with side-effects
- `tak`
	multi-threaded implementation of the Takeuchi function
- `team`
	an implementation of a synchronous concurrency task
