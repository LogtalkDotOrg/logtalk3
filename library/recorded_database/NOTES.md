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


`recorded_database`
===================

The `recorded_database` library aims to help port Prolog code using
the legacy recorded database. Ported applications should still consider
migrating to more standard solutions to handle dynamic data that must
survive backtracking.


API documentation
-----------------

Open the [../../docs/library_index.html#recorded_database](../../docs/library_index.html#recorded_database)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(recorded_database(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(recorded_database(tester)).


Usage
-----

The `recorded_database_core` category implements the library predicates.
This category is imported by the default `recorded_database` object to
provide application global database. To make the database local and thus
minimize potential record clashes, the category can be imported by one
or more application objects. Use protected or private import to restrict
the scope of the library predicates. For example:

	:- object(foo,
		imports(private::recorded_database_core)).

		bar :-
			^^recorda(key, value(1)),
			...

	:- end_object.


Known issues
------------

Currently, references are non-negative integers. They still must be
regarded as opaque terms and subject to change without notice. But
using integers can result in integer overflows when running on backends
with bounded integers in applications performing a large number of
database updates.
