________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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


`grammars`
==========

This library provides Definite Clause Grammars (DCGs) for common parsing
tasks. The DCGs support parsing both lists of characters (aka chars) and
lists of character codes (aka codes).

Currently, three groups of DCGs are available, each defined in its own
file:

- blanks       (`blank_grammars.lgt`)
- numbers      (`number_grammars.lgt`)
- IP addresses (`ip_grammars.lgt`; depends on `number_grammars.lgt`)


API documentation
-----------------

Open the [../../docs/library_index.html#grammars](../../docs/library_index.html#grammars)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(grammars(loader)).


Testing
-------

Minimal tests for this library predicates can be run by loading the
`tester.lgt` file:

	| ?- logtalk_load(grammars(tester)).


Usage
-----

The library uses parametric objects where the single parameter can be either
`chars` or `codes`. The parameter must be bound when using the DCGs. For
example, when using implicit message sending:

	:- uses(blank_grammars(chars), [
		white_spaces//0, new_lines//0
	]).
