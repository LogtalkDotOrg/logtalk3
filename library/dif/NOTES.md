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


`dif`
=====

The `dif` object provides a portable abstraction over how the `dif/2`
predicate is made available by the supported backend Prolog systems
that implement it (B-Prolog, ECLiPSe, XVM, SICStus Prolog, SWI-Prolog,
Trealla Prolog, XSB, and YAP).

Calls to the library predicates are inlined when compiled with the
`optimize` flag turned on. In this case, there is no overhead compared
with calling the abstracted predicate directly.

See also the `coroutining` library.


API documentation
-----------------

Open the [../../docs/library_index.html#dif](../../docs/library_index.html#dif)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(dif(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(dif(tester)).


Usage
-----

Load this library from your application loader file. To call the `dif/1-2`
predicates using implicit message-sending, add the following directive to
any object or category calling the predicates:

	:- uses(dif, [
		dif/2, dif/1
	]).
