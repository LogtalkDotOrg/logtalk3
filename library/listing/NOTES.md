________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


`listing`
=========

This library provides support for listing object dynamic predicates clauses.
The predicates must (also) be declared using a scope directive to make them
visible to the listing predicates.


API documentation
-----------------

Open the [../../docs/library_index.html#listing](../../docs/library_index.html#listing)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(listing(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(listing(tester)).


Usage
-----

This library provides a `listing` category that can be imported by any number
of objects. The main predicates are declared public. If you want to restrict
their scope, use protected or private import. For example:

	:- object(data_store,
		imports(private::listing)).

		...

	:- end_object.

The `listing` category provides a bare bones `portray_clause/1` predicate
implementation. As this predicate is called (by the `listing/0-1` predicates)
using the `(::)/1` control construct, the object importing the category can
easily override the inherited definition with its own or with a call to the
backend system native implementation of the predicate. For example, assuming
a backend that provides `portray_clause/1` as a built-in predicate (e.g. GNU
Prolog), we can write:

	:- object(thing,
		imports(listing)).

		:- uses(user, [portray_clause/1]).
		...

	:- end_object.
