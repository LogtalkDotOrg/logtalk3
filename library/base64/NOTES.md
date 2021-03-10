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


`base64`
========

The `base64` library provides predicates for parsing and generating data in
the Base64 format as per the specification found at:

https://tools.ietf.org/html/rfc4648


API documentation
-----------------

Open the [../../docs/library_index.html#base64](../../docs/library_index.html#base64)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(base64(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(base64(tester)).


Encoding
--------

Encoding a list of bytes in Base64 format is accomplished by the ``generate/2``
predicate. For example:

	| ?- atom_codes('Hello world!', Bytes), base64::generate(atom(Base64), Bytes).
	Base64 = 'SGVsbG8gd29ybGQh'
	Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
	yes

	| ?- atom_codes('Hello world!', Bytes), base64::generate(codes(Base64), Bytes).
	Base64 = [83,71,86,115,98,71,56,103,100,50,57,121,98,71,81,104]
	Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
	yes
	
The Base64 result can also be represented using a list of chars, written to
a file or to a stream. See the API documentation for details.


Decoding
--------

Decoding is accomplished using the ``parse/2`` predicate. For example:

	| ?- base64::parse(atom('SGVsbG8gd29ybGQh'), Bytes), atom_codes(Atom, Bytes).
	Atom = 'Hello world!'
	Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
	yes

	| ?- base64::parse(chars(['S','G','V',s,b,'G','8',g,d,'2','9',y,b,'G','Q',h]), Bytes), atom_codes(Atom, Bytes).
	Atom = 'Hello world!'
	Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
	yes

The `parse/2` predicate accepts other input source such as a file or a stream.
See the API documentation for details.
