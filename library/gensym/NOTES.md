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


`gensym`
========

The `gensym` library implements predicates for generating unique atoms. The
public predicates are declared synchronized to prevent race conditions when
using a backend Prolog compiler with multi-threading support.


API documentation
-----------------

Open the [../../apis/library_index.html#gensym](../../apis/library_index.html#gensym)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(gensym(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(gensym(tester)).


Usage
-----

The `gensym_core` category implements the library predicates. This category
is imported by the default `gensym` object to provide application global
generators. To make the generators local and thus minimize the potential
for generator name clashes, the category can be imported by one or more
application objects. Use protected or private import to restrict the scope
of the library predicates. For example:

	:- object(foo,
		imports(private::gensym_core)).

		bar :-
			^^gensym(p, S),
			...

	:- end_object.
