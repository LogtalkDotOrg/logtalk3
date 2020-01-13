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


`types`
=======

This library implements predicates over standard Prolog term types and 
also term representing common data structures such as lists and pairs.

It also includes a user-extensible `type` object defining type checking
predicates over common Logtalk and Prolog term types.


API documentation
-----------------

Open the [../../docs/library_index.html#types](../../docs/library_index.html#types)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(types(loader)).

In case your code only requires the most basic types, you can load in
alternative the file:

	| ?- logtalk_load(basic_types(loader)).
