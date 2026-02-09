________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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


`deques`
========

This library implements double-ended queues, deques. The queue representation
should be regarded as an opaque term and only accessed using this library
predicates.


API documentation
-----------------

Open the [../../apis/library_index.html#deques](../../apis/library_index.html#deques)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(deques(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(deques(tester)).


Usage
-----

To create a new deque, use the `new/1` predicate:

	| ?- deque::new(Deque).
	Deque = ...
	yes

Elements can be added and removed both at the front and at the back of the
deque using the `push_front/3`, `push_back/3`, `pop_front/3`, and `pop_back/3`,
predicates. For example:

	| ?- deque::(as_deque([1,2,3], Deque0), push_front(0, Deque0, Deque)).
	Deque0 = ...,
	Deque = ...
	yes

We can also peek at the front and back of the deque using the `peek_front/2`
and `peek_back/2` predicates. For example:

	| ?- deque::(as_deque([1,2,3], Deque), peek_back(Deque, Element)).
	Deque = ...,
	Element = 3
	yes

Mapping a closure over all elements of a deque can be done using the `map/2`
and `map/3` predicates. For example:

	| ?- deque::(as_deque([1,2,3], Deque), map(write, Deque)).
	123
	Deque = ...
	yes

For details on these and other provided predicates, consult the library
API documentation.
