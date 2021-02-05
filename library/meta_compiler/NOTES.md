________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


`meta_compiler`
===============

This library supports implementations optimized compilation of meta-calls
for the predicates defined in the `meta` library. 


API documentation
-----------------

Open the [../../docs/library_index.html#meta_compiler](../../docs/library_index.html#meta_compiler)
link in a web browser.


Loading
-------

To load the main entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(meta_compiler(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(meta_compiler(tester)).


Usage
-----

If `meta_compiler` is the only hook object you are using, you can set it as
the default hook object (but note that the optimizations are only applied
to entities compiled with the `optimize` flag turned on):

	| ?- set_logtalk_flag(hook, meta_compiler).
	...

Otherwise, use the `hook(meta_compiler)` and `optimize(on)` complier options
when compiling and loading the code that you want to optimize. For example:

	| ?- logtalk_load(my_source_file, [hook(meta_compiler), optimize(on)]).
	...


See also the `metapredicates_compiled` example and unit tests.
