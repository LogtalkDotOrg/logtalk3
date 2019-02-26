________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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


About
-----

This library provides implementations of common meta-predicates.

The `meta` object implements common Prolog meta-predicates like `map/3` and
`fold_left/4`.

The `loop` object implements several kinds of loops typical of imperative 
languages.

The `meta_compiler` object provides a compiler for the `meta` object
meta-predicates. Is used as an hook object to generate auxiliary predicates
in order to avoid meta-call overheads.


API documentation
-----------------

Open the [../docs/index.html](../docs/index.html) file in a web browser
and choose the library index.


Loading
-------

To load the main entities in this group load the `metapredicates_loader.lgt`
utility file:

	| ?- logtalk_load(library(metapredicates_loader)).

To load also the meta-predicates compiler, load the `meta_compiler_loader.lgt`
utility file:

	| ?- logtalk_load(library(meta_compiler_loader)).


Usage
-----

See the `metapredicates` example and unit tests. To use the meta-predicates
compiler, declare `meta_compiler` as the default hook object or use the
`hook(meta_compiler)` and `optimize(on)` complier options when compiling and
loading the code that you want to optimize.
