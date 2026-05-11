________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


`json_pointer`
==============

The `json_pointer` library provides predicates for parsing, generating,
and evaluating JSON Pointers as specified by RFC 6901:

- https://www.rfc-editor.org/rfc/rfc6901

It supports both the plain string syntax and the URI fragment syntax.
Reference tokens can be represented as atoms, `chars(List)`, or
`codes(List)`.


API documentation
-----------------

Open the [../../apis/library_index.html#json_pointer](../../apis/library_index.html#json_pointer)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(json_pointer(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(json_pointer(tester)).


Representation
--------------

The library defines the `json_pointer(_Representation_)` parametric object
where `_Representation_` can be one of:

- `atom` - reference tokens are represented as atoms
- `chars` - reference tokens are represented as `chars(List)`
- `codes` - reference tokens are represented as `codes(List)`

When using the default `json_pointer` object, reference tokens are
represented as atoms.


Examples
--------

Parse and evaluate a pointer:

	| ?- json_pointer::parse(atom('/foo/0'), Pointer),
	     json_pointer::evaluate(Pointer, {foo-[bar, baz]}, Value).
	Pointer = [foo, '0']
	Value = bar
	yes

Generate a URI fragment representation:

	| ?- json_pointer::generate_fragment(atom(Fragment), ['a b', 'c/d']).
	Fragment = '#/a%20b/c~1d'
	yes
