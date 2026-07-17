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


`json_patch`
============

The `json_patch` library provides predicates for applying JSON Patch
documents as specified by RFC 6902:

- https://www.rfc-editor.org/rfc/rfc6902

Patch documents are represented as lists of JSON operation objects using
the repository native JSON term representation.


API documentation
-----------------

Open the [../../apis/library_index.html#json_patch](../../apis/library_index.html#json_patch)
link in a web browser.


Loading
-------

To load all entities in this library from the repository checkout, load the
`loader.lgt` file from this directory:

	| ?- logtalk_load(json_patch(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(json_patch(tester)).


Representation
--------------

Patch documents are JSON arrays represented as Prolog lists. Each element in
the list is a JSON object representing a patch operation. The public predicate
provided by this library is:

	| ?- json_patch::apply(Patch, OldJSON, NewJSON).

where `Patch` is a list of operation objects, `OldJSON` is the input JSON term,
and `NewJSON` is the patched JSON term.


Patch document structure
------------------------

Each patch operation is encoded as a JSON object using any of the JSON object
representations supported by the repository JSON libraries:

- Curly objects using dash pairs, e.g. `{op-add, path-'/foo', value-1}`
- List objects using `json([...])`, e.g. `json([op=add, path='/foo', value=1])`
- List objects using colon pairs, e.g. `json([':'(op, add), ':'(path, '/foo')])`

The operation member names are always:

- `op`
- `path`
- `from`
- `value`

The `path` and `from` members are JSON Pointer strings parsed using the
`json_pointer` library. They can be represented as:

- atoms, e.g. `'/items/0'`
- `chars(List)`, e.g. `chars(['/', i, t, e, m, s, '/', '0'])`
- `codes(List)`, e.g. `codes([0'/, 0'i, 0't, 0'e, 0'm, 0's, 0'/, 0'0])`

For example:

	| ?- Patch = [{op-add, path-'/baz', value-1}],
	     json_patch::apply(Patch, {foo-bar}, JSON).
	Patch = [{op-add,path-'/baz',value-1}]
	JSON = {foo-bar, baz-1}
	yes


Valid operations
----------------

The library accepts the six RFC 6902 operations:

- `add`
  Required members: `op`, `path`, `value`
  Adds a value at the target path. When the target path is empty (`''`), the
  whole document is replaced by the given value.

		| ?- Patch = [{op-add, path-'/items/-', value-b}],
		     json_patch::apply(Patch, {items-[a]}, JSON).
		Patch = [{op-add,path-'/items/-',value-b}]
		JSON = {items-[a,b]}
		yes

- `remove`
  Required members: `op`, `path`
  Removes the value at the target path. The target must exist. Removing the
  root document is not supported, so an empty path fails.

		| ?- Patch = [{op-remove, path-'/foo'}],
		     json_patch::apply(Patch, {foo-bar}, JSON).
		Patch = [{op-remove,path-'/foo'}]
		JSON = {}
		yes

- `replace`
  Required members: `op`, `path`, `value`
  Replaces the existing value at the target path. When the target path is
  empty, the whole document is replaced.

		| ?- Patch = [{op-replace, path-'', value-[1,2]}],
		     json_patch::apply(Patch, {foo-bar}, JSON).
		Patch = [{op-replace,path-'',value-[1,2]}]
		JSON = [1,2]
		yes

- `move`
  Required members: `op`, `from`, `path`
  Reads the value identified by `from`, removes it from the source location,
  and then adds it at `path`.

		| ?- Patch = [{op-move, from-'/foo', path-'/baz'}],
		     json_patch::apply(Patch, {foo-bar}, JSON).
		Patch = [{op-move,from-'/foo',path-'/baz'}]
		JSON = {baz-bar}
		yes

- `copy`
  Required members: `op`, `from`, `path`
  Reads the value identified by `from` and adds a copy of it at `path`.

		| ?- Patch = [{op-copy, from-'/foo', path-'/baz'}],
		     json_patch::apply(Patch, {foo-bar}, JSON).
		Patch = [{op-copy,from-'/foo',path-'/baz'}]
		JSON = {foo-bar,baz-bar}
		yes

- `test`
  Required members: `op`, `path`, `value`
  Succeeds only if the value at the target path is structurally equal to the
  given `value`. On success, `NewJSON` is unified with the original document.

		| ?- Patch = [{op-test, path-'/flag', value- @true}],
		     json_patch::apply(Patch, {flag- @true}, JSON).
		Patch = [{op-test,path-'/flag',value- @true}]
		JSON = {flag- @true}
		yes


Path rules
----------

- Paths use RFC 6901 JSON Pointer syntax.
- The empty path (`''`) addresses the whole JSON document.
- Array insertion with `add` supports the special `'-'` token for appending to
  the end of an array.
- Array indices must be decimal integers without leading zeros, except for the
  single digit `0`.
- Intermediate containers must already exist. Operations do not create missing
  parent objects or arrays.


Representation notes
--------------------

This implementation reuses `json_pointer` for parsing patch paths and preserves
the object representation found in the patched JSON term:

- curly objects stay curly objects
- `json([...])` objects stay `json([...])` objects
- key text representation such as atoms, `chars(List)`, or `codes(List)` is
  preserved when new members are added to an existing object

The implementation also accepts patch operation objects encoded using curly or
`json([...])` object notation and pointer text represented as atoms,
`chars(List)`, or `codes(List)`.


Failure and errors
------------------

- `apply/3` throws an `instantiation_error` when `Patch` or `OldJSON` is a
  variable.
- `apply/3` throws `type_error(list, Patch)` when the patch document is not a
  list.
- `apply/3` throws `domain_error(json_patch_operation, Operation)` when an
  operation object is malformed.
- Operations fail when a required target value does not exist or when a `test`
  comparison does not succeed.

Empty patch paths are supported for root replacement, copying to the root,
moving to the root, and testing the full document.
