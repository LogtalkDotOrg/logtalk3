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


`types`
=======

This library implements predicates over standard Prolog term types and 
also term representing common data structures such as lists and pairs.

It also includes a user-extensible `type` object defining type checking
predicates over common Logtalk and Prolog term types. The types define
a hierarchy with the Prolog type `term` at the root (i.e. type-checking
a predicate argument of type `term` trivially succeeds). Some types are
only meaningful for backend Prolog systems supporting non-universal
features (e.g. `cyclic` or `char(CharSet)` with a Unicode character set).
See the API documentation for a full list of the types defined by default.


API documentation
-----------------

Open the [../../docs/library_index.html#types](../../docs/library_index.html#types)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(types(loader)).

In case your code only requires the most basic types, you can load in
alternative the file:

	| ?- logtalk_load(basic_types(loader)).

See the notes on the `basic_types` virtual library for details.


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(types(tester)).
