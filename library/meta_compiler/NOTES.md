________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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
file in a web browser.


Loading
-------

To load the main entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(meta_compiler(loader)).


Usage
-----

To use the meta-predicates compiler, declare `meta_compiler` as the default
hook object or use the `hook(meta_compiler)` and `optimize(on)` complier
options when compiling and loading the code that you want to optimize.

See also the `metapredicates_compiled` example and unit tests.
