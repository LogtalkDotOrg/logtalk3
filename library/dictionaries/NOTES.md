________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


About
-----

This library provides a dictionary (also know as associative array, map,
or symbol table) protocol and binary tree, AVL tree, and Red–Black tree
implementations.


API documentation
-----------------

Open the [../../docs/library_index.html#dictionaries](../../docs/library_index.html#dictionaries)
file in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(dictionaries(loader)).


Credits
-------

The AVL tree implementation is an adaptation to Logtalk of the `assoc`
SWI-Prolog library authored by R.A.O'Keefe, L.Damas, V.S.Costa, Glenn
Burgess, Jiri Spitz, and Jan Wielemaker. Additional predicates authored
by Paulo Moura.

The Red–Black tree implementation is an adaptation to Logtalk of the
`rbtrees` Prolog library authored by Vitor Santos Costa.
