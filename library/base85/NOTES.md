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


`base85`
========

The `base85` library provides predicates for encoding and decoding data in
the Base85 (Ascii85) format. Ascii85 is used in PostScript and PDF files.

The encoding uses printable ASCII characters from `!` (33) to `u` (117).
A special shortcut `z` represents four zero bytes.


API documentation
-----------------

Open the [../../apis/library_index.html#base85](../../apis/library_index.html#base85)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(base85(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(base85(tester)).


Encoding
--------

Encoding a list of bytes in Base85 format is accomplished by the
`base85::generate/2` predicate. For example:

	| ?- atom_codes('Man ', Bytes),
	     base85::generate(atom(Base85), Bytes).
	Base85 = '9jqo^'
	Bytes = [77,97,110,32]
	yes

Four zero bytes are encoded as the special character `z`:

	| ?- base85::generate(atom(Base85), [0, 0, 0, 0]).
	Base85 = 'z'
	yes


Decoding
--------

Decoding of Base85 data is accomplished using the `base85::parse/2` predicate.
For example:

	| ?- base85::parse(atom('9jqo^'), Bytes),
	     atom_codes(Atom, Bytes).
	Atom = 'Man '
	Bytes = [77,97,110,32]
	yes

The parser also accepts input with optional `<~` and `~>` delimiters:

	| ?- base85::parse(atom('<~9jqo^~>'), Bytes),
	     atom_codes(Atom, Bytes).
	Atom = 'Man '
	Bytes = [77,97,110,32]
	yes

