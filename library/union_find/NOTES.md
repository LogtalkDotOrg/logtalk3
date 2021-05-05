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


`queues`
========

This library implements queues. The queue representation should be regarded
as an opaque term and only accessed using the library predicates.


API documentation
-----------------

Open the [../../docs/library_index.html#union_find](../../docs/library_index.html#union_find)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(union_find(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(union_find(tester)).


Usage
-----

To create a new union find structure, use the `new/1` predicate:

	| ?- union_find::new(UnionFind).
	UnionFind = ...
	yes

Elements can be added to either the end of the queue or the front of the
queue using, respectively, the `join/3` and `join_all/3` predicates or the
`jump/3` and `jump_all/3`. For example:

	| ?- queue::(new(UnionFind0), join_all([1,2,3], UnionFind0, UnionFind1)).
	UnionFind0 = ...,
	UnionFind1 = ...
	yes
	
We can query the head of the queue or remove the head of the queue using,
respectively, the `head/2` and `serve/3` predicates. For example:

	| ?- queue::(new(Queue0), join(1, Queue0, Queue1), head(Queue1, Head)).
	Queue0 = ...,
	Queue1 = ...,
	Head = 1
	yes

For details on these and other provided predicates, consult the library
API documentation.
