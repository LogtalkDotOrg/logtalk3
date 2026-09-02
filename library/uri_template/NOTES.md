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


`uri_template`
==============

This library implements URI template validation, variable enumeration, and
expansion as specified by RFC 6570. It supports all four template levels,
including all defined operators, prefix modifiers, and exploded list and
associative-array values.

The predicates are defined in the `uri_template(_Representation_)` parametric
object, where `_Representation_` must be one of:

- `atom` - text is represented by atoms
- `chars` - text is represented by lists of characters
- `codes` - text is represented by lists of character codes


API documentation
-----------------

Open the [../../apis/library_index.html#uri-template](../../apis/library_index.html#uri-template)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(uri_template(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(uri_template(tester)).

The test suite includes the complete RFC 6570 example matrix, ported to run
with all three supported text representations. ASCII tests are shared by all
backends. Unicode tests are kept in a separate file and are included only when
the backend reports Unicode support.


Bindings
--------

The `expand/3` predicate accepts a list of unique `Name-Value` pairs. Names and
all textual values use the object representation. Values use one of these
explicit forms:

- `undefined`
- `string(Text)`
- `list(Texts)`
- `assoc(NameValuePairs)`
- `structure(NameValuePairs)`

List members and association values may be `undefined`. Missing bindings are
also undefined. An empty string is defined, while an empty list or association
is undefined. Prefix modifiers are only applicable to string values.

Structured values may contain nested `structure/1` values. They are processed
as associative arrays, with dots separating nested field names as specified by
RFC 6570.

Explicit constructors are necessary because, with `chars` and `codes`, both an
empty text value and an empty composite value would otherwise be represented by
the empty list.


Usage
-----

	| ?- uri_template(atom)::valid('http://example.com/search{?q,lang}').
	true.

	| ?- uri_template(atom)::variables('{x}{?y,x,list*}', Variables).
	Variables = [x, y, list].

	| ?- uri_template(atom)::expand(
	         'http://example.com/search{?q,lang}',
	         [q-string('URI templates'), lang-string(en)],
	         URI
	     ).
	URI = 'http://example.com/search?q=URI%20templates&lang=en'.

	| ?- uri_template(atom)::expand(
	         '/colors{/colors*}',
	         [colors-list([red, green, blue])],
	         URI
	     ).
	URI = '/colors/red/green/blue'.

	| ?- uri_template(atom)::expand(
	         '/search{?parameters*}',
	         [parameters-assoc([q-'URI templates', page-'2'])],
	         URI
	     ).
	URI = '/search?q=URI%20templates&page=2'.

Variable names are case-sensitive. Percent-encoded triplets in variable names
are preserved and are not decoded for binding lookup. Non-ASCII expansion data
is encoded as UTF-8 before percent encoding. The library does not normalize
Unicode; callers should apply NFC normalization to user-provided text when
appropriate, as recommended by RFC 6570.

The `expand/4` predicate provides diagnostic expansion. It returns syntax
errors as zero-based `error(Position, Reason)` terms while preserving malformed
template text according to the recovery recommendations in RFC 6570. The
possible reasons are `malformed_expression`, `unterminated_expression`, and
`invalid_literal`.
