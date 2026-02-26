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


`ksuid`
=======

This library generates random KSUID identifiers:

https://github.com/segmentio/ksuid

**This library requires a backend supporting unbounded integer arithmetic.**

By default, identifiers are represented as atoms and encoded using the
canonical Base62 alphabet:

    0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz

The generation of random identifiers uses the `/dev/urandom` random number
generator when available. This includes macOS, Linux, *BSD, and other POSIX
operating systems. On Windows, a pseudo-random generator is used, randomized
using the current wall time.

Identifiers can be generated as atoms, lists of characters, or lists of
character codes.

See also the `cuid2`, `ids`, `nanoid`, `snowflakeid`, `uuid`, and `ulid` libraries.


API documentation
-----------------

Open the [../../apis/library_index.html#ksuid](../../apis/library_index.html#ksuid)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(ksuid(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(ksuid(tester)).


Usage
-----

To generate a KSUID using the default configuration:

	| ?- ksuid::generate(KSUID).
	KSUID = '2YBXxVf8R5A1x6Yx5Y1AL7bEmel'
	yes

To generate a KSUID represented as a list of characters:

	| ?- ksuid(chars, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')::generate(KSUID).
	KSUID = ['2','Y','B','X','x','V','f','8','R','5','A','1','x','6','Y','x','5','Y','1','A','L','7','b','E','m','e','l']
	yes
