________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains several examples of using threaded engines:

- `ebench`
	benchmarking support for evaluating the performance of threaded
	engines creation and destroying
- `emetas`  
	examples of meta-predicates implemented using engines
- `fluents`  
	examples of defining fluents using engines
- `interactors`  
	examples of defining interactors using engines
- `lazy`  
	examples of defining lazy predicates using engines and coroutining
- `pmq`
	threaded engine implementation of a priority message queue showing
	how to use a perpetual engine to hold an arbitrary structure
- `sums`  
	example of using engines to accumulate state
- `tbbt`  
	examples of defining simple agents using engines
	(inspired by the "The Big Bang Theory" sitcom)
- `whisper`  
	example of creating a chain of engines passing a term
- `yield`  
	example of fetching answers from an engine returned using the
	yield predicate

Threaded engines are only supported on some Prolog compilers. Currently
these include ECLIPSe, SWI-Prolog, Trealla Prolog, XVM, and YAP. 
