________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


`genint`
========

The `genint` library implements predicates for generating positive integers
in increasing order. The public predicates are declared synchronized to
prevent race conditions when using a backend Prolog compiler with
multi-threading support.


API documentation
-----------------

Open the [../../docs/library_index.html#genint](../../docs/library_index.html#genint)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(genint(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(genint(tester)).


Usage
-----

The `genint_core` category implements the library predicates. This category
is imported by the default `genint` object to provide application global
named counters. To make the counters local and thus minimize the potential
for counter name clashes, the category can be imported by one of more
application objects. Use protected or private import to restrict the scope
of the library predicates.
