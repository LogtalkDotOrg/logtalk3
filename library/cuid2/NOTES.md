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


`cuid2`
=======

This library generates random Cuid2 identifiers:

https://github.com/paralleldrive/cuid2

By default, identifiers are represented as atoms with 24 symbols and use
a lowercase alphanumeric alphabet:

    abcdefghijklmnopqrstuvwxyz0123456789

Custom size, alphabet, and representation (atoms, lists of characters,
or lists of character codes) are supported using a parametric object.

The generation of random identifiers uses the `/dev/urandom` random number
generator when available. This includes macOS, Linux, *BSD, and other POSIX
operating systems. On Windows, a pseudo-random generator is used, randomized
using the current wall time.

See also the `ids`, `nanoid`, `ksuid`, `snowflakeid`, `uuid`, and `ulid` libraries.


API documentation
-----------------

Open the [../../apis/library_index.html#cuid2](../../apis/library_index.html#cuid2)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(cuid2(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(cuid2(tester)).


Usage
-----

To generate an identifier using the default configuration:

	| ?- cuid2::generate(Cuid2).
	Cuid2 = 'k4f9mdd51t2r9y53i8h4j1bz'
	yes

To generate a 10-symbol identifier represented as a list of characters:

	| ?- cuid2(chars, 10, 'abcdef012345')::generate(Cuid2).
	Cuid2 = ['a','2','f','e','5','c','1','d','0','b']
	yes
