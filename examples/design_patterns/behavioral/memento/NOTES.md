________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
