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


`coroutining`
=============

The `coroutining` object provides a portable abstraction over how common
coroutining predicates are made available by the supported backend Prolog
systems (ECLiPSe, SICStus Prolog, SWI-Prolog, Trealla Prolog, XVM, and YAP)
that provide them. Partial support for XSB is provided (the predicate
`frozen/2` is not available and calls to it fail).

Calls to the library predicates are inlined when compiled with the `optimize`
flag turned on. In this case, there is no overhead compared with calling the
abstracted predicates directly.

See also the `dif` library.


API documentation
-----------------

Open the [../../apis/library_index.html#coroutining](../../apis/library_index.html#coroutining)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(coroutining(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(coroutining(tester)).


Usage
-----

Load this library from your application loader file. To call the coroutining
predicates using implicit message-sending, add the following directive to any
object or category calling the predicates (adjust the list to the predicates
actually called):

	:- uses(coroutining, [
		dif/2, dif/1, freeze/2, frozen/2, when/2
	]).
