---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
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

# metainterpreters

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
in _this_, i.e., in the database of the object importing the category. 

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

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(metainterpreters(loader)).
```

Direct call of `p/1`:

```logtalk
%%table
database::p(X).
```

<!--
X = 1 ;
X = 2.
-->

A meta-interpreter for pure Prolog:

```logtalk
%%table
database::solve(p(X)).
```

<!--
X = 1 ;
X = 2.
-->

A meta-interpreter returning a proof tree:

```logtalk
%%table
database::proof_tree(p(X), Tree).
```

<!--
X = 1, Tree = p(1):- (q(1, a):- (s(1):-true), (t(1, a):-true)), (r(a):-true) ;
X = 2, Tree = p(2):- (q(2, b):- (s(2):-true), (t(2, b):-true)), (r(b):-true).
-->

A meta-interpreter for tracing goal proofs using pure Prolog:

```logtalk
database::trace(p(X)).
```

<!--
1 call: p(_G180)
2 call: q(_G180, _G316)
3 call: s(_G180)
3 exit: s(1)
3 call: t(1, _G316)
3 exit: t(1, a)
2 exit: q(1, a)
2 call: r(a)
2 exit: r(a)
1 exit: p(1)

X = 1 ;
1 redo: p(1)
2 redo: r(a)
2 fail: r(a)
2 redo: q(1, a)
3 redo: t(1, a)
3 fail: t(1, _G316)
3 redo: s(1)
3 exit: s(2)
3 call: t(2, _G316)
3 exit: t(2, b)
2 exit: q(2, b)
2 call: r(b)
2 exit: r(b)
1 exit: p(2)

X = 2 ;
1 redo: p(2)
2 redo: r(b)
2 fail: r(b)
2 redo: q(2, b)
3 redo: t(2, b)
3 fail: t(2, _G316)
3 redo: s(2)
3 exit: s(3)
3 call: t(3, _G316)
3 fail: t(3, _G316)
3 redo: s(3)
3 fail: s(_G180)
2 fail: q(_G180, _G316)
1 fail: p(_G180)

false.
-->

Counting the number of resolution steps:

```logtalk
lists::steps(reverse([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],_), Steps).
```

<!--
Steps = 496.
-->

An expert system rules example:

```logtalk
rules::prove(weather(Weather)).
```

<!--
Weather = raining.
-->

```logtalk
rules::prove(goto(Where)).
```

<!--
Where = cinema.
-->
