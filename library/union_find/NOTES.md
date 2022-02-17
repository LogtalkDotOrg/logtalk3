________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2022 José Antonio Riaza Valverde
Copyright 2022 Paulo Moura <pmoura@logtalk.org>
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


`union_find`
============

This library implements a disjoint-set data structure, aka a union-find
structure. For a discussion on this data structure, see e.g.

https://en.wikipedia.org/wiki/Disjoint-set_data_structure


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

To create a new union-find structure, use the `new/2` predicate:

	| ?- union_find::new(UnionFind).
	UnionFind = ...
	yes

For details on these and other provided predicates, consult the library
API documentation.
