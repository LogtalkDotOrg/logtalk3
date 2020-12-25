________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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
systems (ECLiPSe, SICStus Prolog, SWI-Prolog, and YAP).

Calls to the library predicates are inlined when compiled with the `optimize`
flag turned on. In this case, there is no overhead compared with calling the
abstracted predicates directly.


API documentation
-----------------

Open the [../../docs/library_index.html#coroutining](../../docs/library_index.html#coroutining)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(coroutining(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(coroutining(tester)).
