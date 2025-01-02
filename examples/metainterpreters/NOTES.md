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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example contains simple meta-interpreters for pure Prolog encapsulated 
in Logtalk categories:

- `solver`  
	simple meta-interpreter for pure Prolog

- `proof_tree`  
	simple meta-interpreter for pure Prolog returning the proof 
	tree for successful queries

- `tracer`  
	simple meta-interpreter for pure Prolog that traces proof 
	construction

- `counter`  
	simple meta-interpreter for counting the number of resolution
	steps when proving a goal

To use a meta-interpreter with an object, simply import the corresponding 
category.

Defining meta-interpreters as categories allows the use of the built-in 
predicate `clause/2` to access the clauses of object predicates without 
forcing these predicates to be declared public or protected. Within a 
category, calls to the built-in predicate `clause/2` retrieve clauses
in "this", i.e. in the database of the object importing the category. 

Counting the number of resolution steps can be applied to a naive
implementation of list reversing for computing LIPS (logical inferences
per second). Reversing a list of 30 elements is know to take 496
resolution steps. Therefore, if `T` is the time in seconds that a
system takes to repeat `N` times this list reversal operation, then:

	LIPS = (496 * N) / T

The value of `N` should be large enough to smooth out any timing
fluctuations. Note that this benchmark is mainly of historical
significance. It can be traced to a David Warren's paper about
the Edinburgh Prolog compiler.
