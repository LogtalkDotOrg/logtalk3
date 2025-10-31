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


`timeout`
=========

The `timeout` object provides a portable abstraction over calling a goal
deterministically with a time limit as made available in some form by some
of the supported backend Prolog systems (B-Prolog, ECLiPSe, SICStus Prolog,
SWI-Prolog, Trealla Prolog, XSB, XVM, and YAP).

For better performance, compile calls to this library meta-predicates with
the `optimize` flag turned on so that the meta-arguments, i.e. the goals
that you are timing, are also compiled.


API documentation
-----------------

Open the [../../apis/library_index.html#timeout](../../apis/library_index.html#timeout)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(timeout(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(timeout(tester)).


Known issues
------------

Two tests are currently skipped when using the SWI-Prolog backend as they
cannot be interrupted and generate the expected timeout exceptions due to
tests being run from an `initialization/1` directive goal that, in the
SWI-Prolog implementation of this directive, ignores signals. The test
goals do generate the expected timeout exception when not called from an
`initialization/1` directive.
