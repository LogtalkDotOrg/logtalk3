---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
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

Design pattern:
	Memento

Description:
	"Without violating encapsulation, capture and externalize an
	object's internal state so that the object can be restored
	to this state later."

This pattern can be used with both classes and prototypes.

In this sample implementation, mementos are represented using dynamic
objects holding the saved state. An alternative would be to create
mementos as parametric object proxies with a corresponding parametric
object to trivially access the sate. The protocol for using mementos
would be the same but without using dynamic objects anf object dynamic
predicates. The best solution would depend notably on the need of
mementos to survive backtracking.

The originator state is here simply represented as an opaque term.
It could be represented in a practical case by e.g. a list of pairs
`PredicateIndicator-ListOfClauses` for all the originator dynamic
predicates. Restoring a memento would then become walking the list
and, for each pair, retracting all clauses for the predicate and
then asserting all listed clauses. These operations of saving and
restoring state from mementos are inherently expensive but could be
minimized by saving and restoring only the partial state affected
by a given operation performed on the originator.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/memento/loader')).
```

Sample sequence of operations from _user_, which plays
the role of the caretaker in this design pattern:

```logtalk
originator::(
		set(s1),
		set(s2),
		save_to_memento(Momento1),
		set(s3),
		save_to_memento(Momento2),
		set(s4),
		restore_from_memento(Momento1),
		set(s5),
		restore_from_memento(Momento2)
	).
```

<!--
Originator: Setting state to s1
Originator: Setting state to s2
Originator: Saving to Memento.
Originator: Setting state to s3
Originator: Saving to Memento.
Originator: Setting state to s4
Originator: State after restoring from Memento: s2
Originator: Setting state to s5
Originator: State after restoring from Memento: s3
Momento1 = o1,
Momento2 = o2

true.
-->
