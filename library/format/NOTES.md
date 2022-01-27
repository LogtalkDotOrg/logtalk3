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


`format`
========

The `format` object provides a portable abstraction over how the de
facto standard `format/2-3` predicates are made available by the
supported backend Prolog systems. Some system provide these predicates
as built-in predicates while others make them available using a library
that must be explicitly loaded.

Calls to the library predicates are inlined when compiled with the
`optimize` flag turned on for most of the backends. When that's the
case, there is no overhead compared with calling the abstracted
predicates directly.


Portability
-----------

Some Prolog systems provide only a subset of the expected format specifiers.
Notably, table related format specifiers are only fully supported by a few
systems. See the section below on testing.

Only some of the supported Prolog backends provide implementations of the
`format/2-3` predicates that allow using not only an atom or a list of
character codes for the format string (as de facto standard) but also using
a list of characters. These currently include ECLiPSe, LVM, SICStus Prolog,
SWI-Prolog, Trealla Prolog, and YAP. Therefore, when wide portability is
sought, atoms must be used for the format specifier argument. Some systems,
like Scryer Prolog and Tau Prolog, only accept a list of characters for the
format string. In this case, this library will convert the atom format
string before calling these systems native implementations.


API documentation
-----------------

Open the [../../docs/library_index.html#format](../../docs/library_index.html#format)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(format(loader)).


Testing
-------

Minimal tests for this library predicates can be run by loading the
`tester.lgt` file:

	| ?- logtalk_load(format(tester)).

Detailed tests for the `format/2-3` predicates are available in the
`tests/prolog/predicates` directory as part of the Prolog standards
conformance test suite. Use those tests to confirm the portability
of the format specifiers that you want to use.
