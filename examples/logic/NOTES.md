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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains an object which implements a translator from 
first-order predicate logic propositions to conjunctive normal form 
and to clausal form. The translator code is partially based on code 
published in the book "Programming in Prolog" by W. F. Clocksin and 
C. S. Mellish.

The following operators are used for representing logic connectives:

- negation: `~`
- disjunction: `v`
- conjunction: `&`
- implication: `=>`
- equivalence: `<=>`

Quantifiers are represented using the following notation:

- universal: `all(X, P)`
- existential: `exists(X, P)`

The two main object predicates are `translate/2` and `step_by_step/2`.
The first predicate, `translate/2`, translate a logic proposition to 
a list of clauses. The second predicate, `step_by_step/2`, performs 
the same translations as `translate/2` but also prints the results 
of each conversion step.
