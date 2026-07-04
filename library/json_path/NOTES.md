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


`json_path`
===========

This library provides predicates for parsing, generating, and evaluating
JSONPath queries for native Logtalk JSON terms.

It is based on RFC 9535:

- https://www.rfc-editor.org/rfc/rfc9535


Current implementation status
-----------------------------

This implementation currently supports:

- child segments
- descendant segments
- name selectors
- wildcard selectors
- index selectors, including negative indexes
- slice selectors
- filter selectors
- existence tests
- logical operators: `||`, `&&`, and `!`
- comparisons using literals, singular queries, and supported function expressions
- function expressions: `length()`, `count()`, `match()`, `search()`, and `value()`
- local I-Regexp matching support for alternation, grouping, wildcard `.`, character classes, single-character escapes, and quantifiers (`*`, `+`, `?`, `{m}`, `{m,}`, `{m,n}`)
- Unicode general-category escapes `\p{...}` and `\P{...}` using backend `unicode_property/2` support when available
- additional local validation for malformed character classes and for unsafe explicit range quantifiers (nested explicit ranges and very large upper bounds/spans are rejected)
- validation of index and slice integers against the RFC 9535 I-JSON exact-integer range
- validation that queries supplied in `codes(...)` representation contain only Unicode scalar values
- normalized path reporting

Still pending:

- the remaining RFC 9535 function-extension surface


API documentation
-----------------

Open the [../../apis/library_index.html#json_path](../../apis/library_index.html#json_path)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(json_path(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(json_path(tester)).


Representation
--------------

The library defines the `json_path(_Representation_)` parametric object where
`_Representation_` can be one of:

- `atom` - member names and normalized paths are represented as atoms
- `chars` - member names and normalized paths are represented as `chars(List)`
- `codes` - member names and normalized paths are represented as `codes(List)`

When using the default `json_path` object, member names and normalized paths
are represented as atoms.


Examples
--------

Parse and evaluate a query:

	| ?- json_path::parse(atom('$.store.book[0].title'), Query),
	     json_path::evaluate(Query, {store-{book-[{title-'Sayings of the Century'}]}}, Values).
	Query = json_path([child([name(store)]), child([name(book)]), child([index(0)]), child([name(title)])])
	Values = ['Sayings of the Century']
	yes

Filter books by price:

	| ?- JSON = {store-{book-[{title-'Sayings', price-8.95}, {title-'Honour', price-12.99}]}},
	     json_path::query(atom('$.store.book[?@.price < 10].title'), JSON, Values).
	JSON = {store-{book-[{title-'Sayings', price-8.95}, {title-'Honour', price-12.99}]}}
	Values = ['Sayings']
	yes

Return normalized paths for the selected nodes:

	| ?- json_path::parse(atom('$..j'), Query),
	     json_path::paths(Query, {o-{j-1}, a-[[{j-4}]]}, Paths).
	Paths = ['$[''o''][''j'']', '$[''a''][0][0][''j'']']
	yes
